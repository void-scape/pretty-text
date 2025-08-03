use bevy::prelude::*;

///
/// This inserts the core systems and resources then registers
/// [`pretty_text_effects`]'s built-in effects.
#[derive(Debug)]
pub struct PrettyTextPlugin;

impl Plugin for PrettyTextPlugin {
    fn build(&self, app: &mut App) {
        ui
        app.add_plugins((
            pretty_text::PrettyTextCorePlugin,
            #[cfg(feature = "default_effects")]
            pretty_text_effects::EffectsPlugin,
        ));
    }
}
