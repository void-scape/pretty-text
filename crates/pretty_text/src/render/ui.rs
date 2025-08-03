use core::marker::PhantomData;

use bevy::math::{Mat4, Vec2};
use bevy::render::RenderApp;
use bevy::render::sync_world::TemporaryRenderEntity;
use bevy::render::{
    Extract, ExtractSchedule, Render, RenderSet,
    globals::GlobalsBuffer,
    render_asset::RenderAssets,
    render_phase::*,
    render_resource::*,
    renderer::{RenderDevice, RenderQueue},
    view::*,
};
use bevy::text::{ComputedTextBlock, PositionedGlyph};
use bevy::transform::prelude::GlobalTransform;
use bevy::ui::{RenderUiSystem, TransparentUi, UiCameraMap, extract_text_sections};
use bevy::{ecs::system::*, render::texture::GpuImage};

use crate::effects::material::{GlyphMaterial, PrettyTextMaterial};
use crate::glyph::{Glyph, GlyphSpan, Glyphs};
use crate::render::GlyphVertex;
use crate::*;

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

pub(super) struct GlyphMaterialUiPlugin<T: GlyphMaterial>(PhantomData<T>);

impl<T: GlyphMaterial> Default for GlyphMaterialUiPlugin<T> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<T: GlyphMaterial> Plugin for GlyphMaterialUiPlugin<T> {
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
    uitext_query: Extract<
        Query<(
            Entity,
            &ComputedNode,
            &GlobalTransform,
            Option<&CalculatedClip>,
            &ComputedNodeTarget,
            &ComputedTextBlock,
            &Glyphs,
        )>,
    >,
    glyphs: Extract<Query<(&Glyph, &GlyphSpan, &InheritedVisibility, &Transform)>>,
    text_styles: Extract<Query<&TextColor>>,
    text_materials: Extract<Query<&PrettyTextMaterial<T>>>,
    camera_map: Extract<UiCameraMap>,
) {
    let mut index = extracted_glyphs.len();
    let mut extracted = Vec::new();

    let mut camera_mapper = camera_map.get_mapper();
    for (entity, uinode, global_transform, clip, camera, computed_block, glyph_entities) in
        &uitext_query
    {
        // glyph entities are the source of truth for visibility
        if uinode.is_empty() {
            continue;
        }

        let Some(extracted_camera_entity) = camera_mapper.map(camera) else {
            continue;
        };

        let transform = global_transform.affine()
            * bevy::math::Affine3A::from_translation((-0.5 * uinode.size()).extend(0.));

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
                    transform: transform
                        * Mat4::from_translation(position.extend(0.))
                        // translation is halved during centering
                        * (glyph_transform.with_translation(glyph_transform.translation * 2.0))
                            .compute_affine(),
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
                    kind: ExtractedGlyphSpanKind::Ui {
                        clip: clip.map(|clip| clip.clip),
                        extracted_camera_entity,
                    },
                    sork_key: uinode.stack_index as f32 + bevy::ui::stack_z_offsets::MATERIAL,
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
    if let (Some(view_binding), Some(globals_binding)) = (
        view_uniforms.uniforms.binding(),
        globals_buffer.buffer.binding(),
    ) {
        let mut batches: Vec<(Entity, GlyphBatch<T>)> = Vec::with_capacity(*previous_len);

        glyph_meta.vertices.clear();
        glyph_meta.view_bind_group = Some(render_device.create_bind_group(
            "pretty_text_glyph_view_bind_group",
            &material_pipeline.view_layout,
            &BindGroupEntries::sequential((view_binding, globals_binding)),
        ));

        let mut index = 0;

        for ui_phase in phases.values_mut() {
            let mut batch_item_index = 0;
            let mut batch_shader_handle = AssetId::invalid();
            let mut image_handle = AssetId::invalid();

            for item_index in 0..ui_phase.items.len() {
                let item = &mut ui_phase.items[item_index];
                if let Some(span) = extracted_spans.get(item.index).filter(|n| {
                    item.entity() == n.render_entity
                        && matches!(n.kind, ExtractedGlyphSpanKind::Ui { .. })
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

                        let ExtractedGlyphSpanKind::Ui { clip, .. } = span.kind else {
                            unreachable!()
                        };
                        let positions_diff = if let Some(clip) = clip {
                            [
                                Vec2::new(
                                    f32::max(clip.min.x - positions[0].x, 0.),
                                    f32::max(clip.min.y - positions[0].y, 0.),
                                ),
                                Vec2::new(
                                    f32::min(clip.max.x - positions[1].x, 0.),
                                    f32::max(clip.min.y - positions[1].y, 0.),
                                ),
                                Vec2::new(
                                    f32::min(clip.max.x - positions[2].x, 0.),
                                    f32::min(clip.max.y - positions[2].y, 0.),
                                ),
                                Vec2::new(
                                    f32::max(clip.min.x - positions[3].x, 0.),
                                    f32::min(clip.max.y - positions[3].y, 0.),
                                ),
                            ]
                        } else {
                            [Vec2::ZERO; 4]
                        };

                        let positions_clipped = [
                            positions[0] + positions_diff[0].extend(0.),
                            positions[1] + positions_diff[1].extend(0.),
                            positions[2] + positions_diff[2].extend(0.),
                            positions[3] + positions_diff[3].extend(0.),
                        ];

                        let transformed_rect_size = glyph.transform.transform_vector3(rect_size);

                        // Don't try to cull nodes that have a rotation
                        // In a rotation around the Z-axis, this value is 0.0 for an angle of 0.0 or π
                        // In those two cases, the culling check can proceed normally as corners will be on
                        // horizontal / vertical lines
                        // For all other angles, bypass the culling check
                        // This does not properly handles all rotations on all axis
                        if glyph.transform.x_axis[1] == 0.0 {
                            // Cull nodes that are completely clipped
                            if positions_diff[0].x - positions_diff[1].x >= transformed_rect_size.x
                                || positions_diff[1].y - positions_diff[2].y
                                    >= transformed_rect_size.y
                            {
                                continue;
                            }
                        }

                        let uvs = [
                            Vec2::new(
                                glyph.rect.min.x + positions_diff[0].x,
                                glyph.rect.min.y + positions_diff[0].y,
                            ),
                            Vec2::new(
                                glyph.rect.max.x + positions_diff[1].x,
                                glyph.rect.min.y + positions_diff[1].y,
                            ),
                            Vec2::new(
                                glyph.rect.max.x + positions_diff[2].x,
                                glyph.rect.max.y + positions_diff[2].y,
                            ),
                            Vec2::new(
                                glyph.rect.min.x + positions_diff[3].x,
                                glyph.rect.max.y + positions_diff[3].y,
                            ),
                        ]
                        .map(|pos| pos / atlas_extent);

                        for i in QUAD_INDICES {
                            glyph_meta.vertices.push(GlyphVertex {
                                position: positions_clipped[i].into(),
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
