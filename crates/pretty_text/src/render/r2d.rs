use core::marker::PhantomData;

use bevy::math::{Mat4, Vec2};
use bevy::prelude::*;
use bevy::render::RenderApp;
use bevy::render::sync_world::TemporaryRenderEntity;
use bevy::render::texture::GpuImage;
use bevy::render::{
    Extract, ExtractSchedule, Render, RenderSet,
    globals::GlobalsBuffer,
    render_asset::RenderAssets,
    render_phase::*,
    render_resource::*,
    renderer::{RenderDevice, RenderQueue},
    view::*,
};
use bevy::sprite::Anchor;
use bevy::text::{ComputedTextBlock, PositionedGlyph, TextBounds, TextLayoutInfo};
use bevy::transform::prelude::GlobalTransform;
use bevy::ui::{RenderUiSystem, TransparentUi, extract_text_sections};
use bevy::window::PrimaryWindow;

use crate::effects::material::{GlyphMaterial, PrettyTextMaterial};
use crate::glyph::{Glyph, GlyphSpan, Glyphs};
use crate::render::GlyphVertex;

use super::{
    ExtractedGlyph, ExtractedGlyphSpan, ExtractedGlyphSpanKind, ExtractedGlyphSpans,
    ExtractedGlyphs, GlyphBatch, GlyphMaterialMeta, GlyphMaterialPipeline, ImageBindGroups,
};

const QUAD_VERTEX_POSITIONS: [Vec2; 4] = [
    Vec2::new(-0.5, -0.5),
    Vec2::new(0.5, -0.5),
    Vec2::new(0.5, 0.5),
    Vec2::new(-0.5, 0.5),
];

const QUAD_INDICES: [usize; 6] = [0, 2, 3, 0, 1, 2];

pub(super) struct GlyphMaterial2dPlugin<T: GlyphMaterial>(PhantomData<T>);

impl<T: GlyphMaterial> Default for GlyphMaterial2dPlugin<T> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<T: GlyphMaterial> Plugin for GlyphMaterial2dPlugin<T> {
    fn build(&self, app: &mut App) {
        if let Some(render_app) = app.get_sub_app_mut(RenderApp) {
            render_app
                .add_systems(
                    ExtractSchedule,
                    extract_glyphs::<T>
                        .in_set(RenderUiSystem::ExtractText)
                        .before(extract_text_sections),
                )
                .add_systems(
                    Render,
                    prepare_glyphs::<T>
                        .after(super::prepare_glyph_meta::<T>)
                        .in_set(RenderSet::PrepareBindGroups),
                );
        }
    }
}

// Extract the glyph spans that are rendered with `T`.
fn extract_glyphs<T: GlyphMaterial>(
    mut commands: Commands,
    mut extracted_spans: ResMut<ExtractedGlyphSpans<T>>,
    mut extracted_glyphs: ResMut<ExtractedGlyphs>,
    texture_atlases: Extract<Res<Assets<TextureAtlasLayout>>>,
    windows: Extract<Query<&Window, With<PrimaryWindow>>>,
    text_query: Extract<
        Query<(
            Entity,
            &ComputedTextBlock,
            &TextLayoutInfo,
            &TextBounds,
            &Anchor,
            &GlobalTransform,
            &Glyphs,
        )>,
    >,
    glyphs: Extract<Query<(&Glyph, &GlyphSpan, &InheritedVisibility, &Transform)>>,
    text_styles: Extract<Query<&TextColor>>,
    text_materials: Extract<Query<&PrettyTextMaterial<T>>>,
) {
    let mut index = extracted_glyphs.len();
    let mut extracted = Vec::new();

    let scale_factor = windows
        .single()
        .map(|window| window.resolution.scale_factor())
        .unwrap_or(1.0);
    let scaling = GlobalTransform::from_scale(Vec2::splat(scale_factor.recip()).extend(1.));

    for (entity, computed_block, layout_info, bounds, anchor, global_transform, glyph_entities) in
        &text_query
    {
        let size = Vec2::new(
            bounds.width.unwrap_or(layout_info.size.x),
            bounds.height.unwrap_or(layout_info.size.y),
        );

        let bottom_left = -(anchor.as_vec() + 0.5) * size + (size.y - layout_info.size.y) * Vec2::Y;
        let transform =
            *global_transform * GlobalTransform::from_translation(bottom_left.extend(0.)) * scaling;

        let mut iter = glyphs.iter_many(glyph_entities.iter()).peekable();

        while let Some((
            Glyph(PositionedGlyph {
                position,
                atlas_info,
                span_index,
                ..
            }),
            span_entity,
            inherited_visibility,
            glyph_transform,
        )) = iter.next()
        {
            if inherited_visibility.get() && text_materials.get(span_entity.0).is_ok() {
                let rect = texture_atlases
                    .get(&atlas_info.texture_atlas)
                    .unwrap()
                    .textures[atlas_info.location.glyph_index]
                    .as_rect();
                extracted_glyphs.push(ExtractedGlyph {
                    transform: transform.affine()
                        * Mat4::from_translation(position.extend(0.))
                        * glyph_transform.compute_affine(),
                    rect,
                });
                extracted.push(index);
                index += 1;
            }

            if !extracted.is_empty()
                && iter.peek().is_none_or(|(glyph, _, _, _)| {
                    glyph.0.span_index != *span_index
                        || glyph.0.atlas_info.texture != atlas_info.texture
                })
            {
                let color = text_styles
                    .get(
                        computed_block
                            .entities()
                            .get(*span_index)
                            .map(|t| t.entity)
                            .unwrap_or(Entity::PLACEHOLDER),
                    )
                    .map(|text_color| LinearRgba::from(text_color.0))
                    .unwrap_or_default();
                let material_handle = text_materials.get(span_entity.0).unwrap();

                extracted_spans.push(ExtractedGlyphSpan {
                    kind: ExtractedGlyphSpanKind::Sprite,
                    sork_key: transform.translation().z + glyph_transform.translation.z,
                    main_entity: entity.into(),
                    render_entity: commands.spawn(TemporaryRenderEntity).id(),
                    color: color.to_f32_array(),
                    image: atlas_info.texture.id(),
                    material: material_handle.0.id(),
                    extracted: extracted.drain(..).collect(),
                });
            }
        }
    }
}

fn prepare_glyphs<T: GlyphMaterial>(
    mut commands: Commands,
    render_device: Res<RenderDevice>,
    render_queue: Res<RenderQueue>,
    mut image_bind_groups: ResMut<ImageBindGroups>,
    mut glyph_meta: ResMut<GlyphMaterialMeta<T>>,
    mut extracted_spans: ResMut<ExtractedGlyphSpans<T>>,
    extracted_glyphs: ResMut<ExtractedGlyphs>,
    view_uniforms: Res<ViewUniforms>,
    globals_buffer: Res<GlobalsBuffer>,
    material_pipeline: Res<GlyphMaterialPipeline<T>>,
    gpu_images: Res<RenderAssets<GpuImage>>,
    mut phases: ResMut<ViewSortedRenderPhases<TransparentUi>>,
    mut previous_len: Local<usize>,
) {
    if view_uniforms.uniforms.binding().is_some() && globals_buffer.buffer.binding().is_some() {
        let mut batches: Vec<(Entity, GlyphBatch<T>)> = Vec::with_capacity(*previous_len);
        let mut index = 0;

        for ui_phase in phases.values_mut() {
            let mut batch_item_index = 0;
            let mut batch_shader_handle = AssetId::invalid();
            let mut image_handle = AssetId::invalid();

            for item_index in 0..ui_phase.items.len() {
                let item = &mut ui_phase.items[item_index];
                if let Some(span) = extracted_spans.get(item.index).filter(|n| {
                    item.entity() == n.render_entity
                        && matches!(n.kind, ExtractedGlyphSpanKind::Sprite)
                }) {
                    let image = gpu_images
                        .get(span.image)
                        .expect("Image was checked during batching and should still exist");

                    let mut existing_batch = batches.last_mut().filter(|_| {
                        batch_shader_handle == span.material && image_handle == span.image
                    });

                    if existing_batch.is_none() {
                        batch_item_index = item_index;
                        batch_shader_handle = span.material;
                        image_handle = span.image;

                        image_bind_groups.0.entry(span.image).or_insert_with(|| {
                            render_device.create_bind_group(
                                "pretty_text_texture_bind_group",
                                &material_pipeline.texture_layout,
                                &BindGroupEntries::sequential((
                                    &image.texture_view,
                                    &image.sampler,
                                )),
                            )
                        });

                        let new_batch = GlyphBatch {
                            range: index..index,
                            material: span.material,
                            image: span.image,
                        };

                        batches.push((item.entity(), new_batch));

                        existing_batch = batches.last_mut();
                    }

                    let atlas_extent = image.size_2d().as_vec2();
                    let color = span.color;

                    for &glyph in span.extracted.iter().map(|i| &extracted_glyphs[*i]) {
                        let glyph_rect = glyph.rect;
                        let rect_size = glyph_rect.size().extend(1.0);

                        // Specify the corners of the glyph
                        let positions = QUAD_VERTEX_POSITIONS.map(|pos| {
                            (glyph.transform * (pos.extend(0.) * rect_size).extend(1.)).xyz()
                        });

                        let uvs = [
                            Vec2::new(glyph.rect.min.x, glyph.rect.min.y),
                            Vec2::new(glyph.rect.max.x, glyph.rect.min.y),
                            Vec2::new(glyph.rect.max.x, glyph.rect.max.y),
                            Vec2::new(glyph.rect.min.x, glyph.rect.max.y),
                        ]
                        .map(|pos| pos / atlas_extent);

                        for i in QUAD_INDICES {
                            glyph_meta.vertices.push(GlyphVertex {
                                position: positions[i].into(),
                                uv: uvs[i].into(),
                                color,
                            });
                        }

                        index += QUAD_INDICES.len() as u32;
                        match &mut existing_batch {
                            Some(existing_batch) => existing_batch.1.range.end = index,
                            None => unreachable!(),
                        }
                    }
                    ui_phase.items[batch_item_index].batch_range_mut().end += 1;
                } else {
                    batch_shader_handle = AssetId::invalid();
                    image_handle = AssetId::invalid();
                }
            }
        }

        glyph_meta
            .vertices
            .write_buffer(&render_device, &render_queue);

        *previous_len = batches.len();
        commands.try_insert_batch(batches);
    }

    extracted_spans.clear();
}
