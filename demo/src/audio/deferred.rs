#![expect(dead_code)]

use bevy_seedling::fixed_vec::FixedVec;
use firewheel::{
    clock::ClockSeconds,
    diff::{Diff, EventQueue, Patch, PatchError, PathBuilder},
    event::ParamData,
};

/// A parameter expressed as a timeline of events.
///
/// This allows parameters to vary smoothly at audio-rate
/// with minimal cross-thread communication.
#[derive(Debug, Clone)]
pub struct Deferred<T> {
    value: T,
    events: FixedVec<DeferredEvent<T>>,
    /// The total number of events consumed.
    consumed: usize,
}

impl<T> Deferred<T> {
    /// Create a new instance of [`Timeline`] with an initial value.
    pub fn new(value: T) -> Self {
        Self {
            value,
            events: Default::default(),
            consumed: 0,
        }
    }

    /// Remove all events from the timeline.
    pub fn clear(&mut self) {
        self.events.clear();
    }
}

impl<T: Clone> Deferred<T> {
    /// Push an event to the timeline, popping off the oldest one if the
    /// queue is full.
    pub fn push(&mut self, event: DeferredEvent<T>) {
        // scan the events to ensure the event doesn't overlap any ranges
        match &event {
            DeferredEvent::Immediate(i) => {
                self.clear();
                self.value = i.clone();
            }
            _ => {}
        }

        self.events.push(event);
        self.consumed += 1;
    }

    /// Set the value immediately.
    pub fn set(&mut self, value: T) {
        // This push cannot fail.
        self.push(DeferredEvent::Immediate(value));
    }

    /// Push a curve event with absolute timestamps.
    pub fn set_at(&mut self, value: T, time: ClockSeconds) {
        self.push(DeferredEvent::Deferred { value, time });
    }

    /// Get the value at a point in time.
    pub fn value_at(&self, time: ClockSeconds) -> T {
        if let Some(bounded) = self.events.iter().find(|e| e.contains(time)) {
            return bounded.get();
        }

        let mut recent_time = f64::MAX;
        let mut recent_value = None;

        for event in self.events.iter() {
            if let Some(end) = event.end_time() {
                let delta = time.0 - end.0;

                if delta >= 0. && delta < recent_time {
                    recent_time = delta;
                    recent_value = Some(event.end_value());
                }
            }
        }

        recent_value.unwrap_or(self.value.clone())
    }

    /// Get the current value without respect to time.
    ///
    /// This depends on regular calls to [`Timeline::tick`]
    /// for accuracy.
    pub fn get(&self) -> T {
        self.value.clone()
    }

    /// Update the inner value to the current timestamp.
    pub fn tick(&mut self, now: ClockSeconds) {
        self.value = self.value_at(now);
    }
}

/// A single timeline event.
#[derive(Debug, Clone)]
pub enum DeferredEvent<T> {
    /// An immediate event, which also clears the timeline buffer.
    Immediate(T),
    /// A deferred event.
    Deferred {
        /// The target value.
        value: T,
        /// The time at which this value should be written.
        time: ClockSeconds,
    },
}

impl<T> DeferredEvent<T> {
    /// This event's start time, if any.
    pub fn start_time(&self) -> Option<ClockSeconds> {
        match self {
            Self::Deferred { time, .. } => Some(*time),
            _ => None,
        }
    }

    /// This event's end time, if any.
    ///
    /// A [`TimelineEvent::Deferred`] variant will
    /// provide its start time.
    pub fn end_time(&self) -> Option<ClockSeconds> {
        match self {
            Self::Deferred { time, .. } => Some(*time),
            _ => None,
        }
    }

    /// Returns true if the event contains the given time.
    pub fn contains(&self, time: ClockSeconds) -> bool {
        match self {
            Self::Deferred { time: t, .. } => *t == time,
            _ => false,
        }
    }
}

impl<T: Clone> DeferredEvent<T> {
    /// Calculates the value at `time`.
    pub fn get(&self) -> T {
        match self {
            Self::Immediate(i) => i.clone(),
            Self::Deferred { value, .. } => value.clone(),
        }
    }

    /// Gets the starting value.
    pub fn start_value(&self) -> T {
        match self {
            Self::Immediate(i) => i.clone(),
            Self::Deferred { value, .. } => value.clone(),
        }
    }

    /// Gets the ending value.
    ///
    /// [`TimelineEvent::Immediate`] and [`TimelineEvent::Deferred`]
    /// will simply return their single value.
    pub fn end_value(&self) -> T {
        match self {
            Self::Immediate(i) => i.clone(),
            Self::Deferred { value, .. } => value.clone(),
        }
    }
}

impl<T: Clone + Send + Sync + 'static> Diff for Deferred<T> {
    fn diff<E: EventQueue>(&self, baseline: &Self, path: PathBuilder, event_queue: &mut E) {
        let newly_consumed = self.consumed.saturating_sub(baseline.consumed);
        if newly_consumed == 0 {
            return;
        }

        // If more items were added than the buffer can hold, we only have the most recent self.events.len() items.
        let clamped_newly_consumed = newly_consumed.min(self.events.len());

        // Start index for the new items. They are the last 'clamped_newly_consumed' items in the buffer.
        let start = self.events.len() - clamped_newly_consumed;
        let new_items = &self.events[start..];

        for event in new_items.iter() {
            event_queue.push_param(ParamData::any(event.clone()), path.clone());
        }
    }
}

impl<T: Clone + 'static> Patch for Deferred<T> {
    type Patch = DeferredEvent<T>;

    fn patch(data: &ParamData, _: &[u32]) -> Result<Self::Patch, PatchError> {
        let value: &DeferredEvent<T> = data.downcast_ref().ok_or(PatchError::InvalidData)?;

        Ok(value.clone())
    }

    fn apply(&mut self, patch: Self::Patch) {
        self.push(patch);
    }
}
