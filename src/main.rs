use alsa::mixer::MilliBel;
use itertools::{Either, Itertools};
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MixerDirection {
    Playback,
    Capture,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MixerVolumeMapper {
    Auto,
    Linear,
}

struct MixerControl<'a> {
    selem: alsa::mixer::Selem<'a>,
    dir: MixerDirection,
    monitor: bool,
    db_range: (MilliBel, MilliBel),
    raw_range: (i64, i64),
    last_volume: Option<i64>,
    scale: f64,
    volume_mapper: MixerVolumeMapper,
}

impl<'a> MixerControl<'a> {
    const DEFAULT_CHANNEL: alsa::mixer::SelemChannelId = alsa::mixer::SelemChannelId::FrontLeft;
    const MAX_LINEAR_DB_SCALE: f64 = 24.0;
    const SND_CTL_TLV_DB_GAIN_MUTE: i64 = -9999999;

    fn new(
        mixer: &'a alsa::Mixer,
        name: &str,
        index: u32,
        dir: MixerDirection,
        monitor: bool,
        scale: f64,
        volume_mapper: MixerVolumeMapper,
    ) -> Self {
        let id = alsa::mixer::SelemId::new(name, index);
        let selem = mixer
            .find_selem(&id)
            .expect(&format!("unable to find selem \"{}\",{}", name, index));

        let db_range = match dir {
            MixerDirection::Playback => selem.get_playback_db_range(),
            MixerDirection::Capture => selem.get_capture_db_range(),
        };

        let raw_range = match dir {
            MixerDirection::Playback => selem.get_playback_volume_range(),
            MixerDirection::Capture => selem.get_capture_volume_range(),
        };

        Self {
            selem,
            dir,
            monitor,
            db_range,
            raw_range,
            scale,
            volume_mapper,
            last_volume: None,
        }
    }

    fn get_volume_raw(&mut self) -> i64 {
        let vol = match self.dir {
            MixerDirection::Playback => self.selem.get_playback_volume(Self::DEFAULT_CHANNEL),
            MixerDirection::Capture => self.selem.get_capture_volume(Self::DEFAULT_CHANNEL),
        }
        .unwrap();

        if self.monitor {
            self.last_volume = Some(vol);
        }

        vol
    }

    fn raw_to_db(&self, raw_vol: i64) -> Option<MilliBel> {
        match self.dir {
            MixerDirection::Playback => self.selem.ask_playback_vol_db(raw_vol),
            MixerDirection::Capture => self.selem.ask_capture_vol_db(raw_vol),
        }
        .ok()
    }

    fn db_to_raw(&self, db_vol: MilliBel) -> Option<i64> {
        match self.dir {
            MixerDirection::Playback => self.selem.ask_playback_db_vol(db_vol, alsa::Round::Floor),
            MixerDirection::Capture => self.selem.ask_capture_db_vol(db_vol, alsa::Round::Floor),
        }
        .ok()
    }

    fn raw_to_norm(&self, raw_vol: i64) -> Option<f64> {
        let (db_min, db_max) = self.db_range;
        let (raw_min, raw_max) = self.raw_range;

        let mut norm_vol;

        if db_min >= db_max || self.volume_mapper == MixerVolumeMapper::Linear {
            if raw_min == raw_max {
                return None;
            }

            norm_vol = ((raw_vol - raw_min) as f64) / ((raw_max - raw_min) as f64);
        } else {
            let db_vol = self.raw_to_db(raw_vol)?;

            if db_max - db_min <= MilliBel::from_db(Self::MAX_LINEAR_DB_SCALE as f32) {
                norm_vol = (db_vol.0 - db_min.0) as f64 / (db_max.0 - db_min.0) as f64;
            } else {
                norm_vol = 10.0_f64.powf((db_vol - db_max).0 as f64 / 6000.0);

                if db_min.0 != Self::SND_CTL_TLV_DB_GAIN_MUTE {
                    let norm_min = 10.0_f64.powf((db_min - db_max).0 as f64 / 6000.0);
                    norm_vol = (norm_vol - norm_min) / (1.0 - norm_min);
                }
            }
        }

        Some((norm_vol / self.scale).clamp(0.0, 1.0))
    }

    fn norm_to_raw(&self, norm_vol: f64) -> Option<i64> {
        let norm_vol = norm_vol * self.scale;
        let (db_min, db_max) = self.db_range;
        let (raw_min, raw_max) = self.raw_range;

        if db_min >= db_max || self.volume_mapper == MixerVolumeMapper::Linear {
            return Some(((norm_vol * ((raw_max - raw_min) as f64)) as i64) + raw_min);
        }

        if db_max - db_min <= MilliBel::from_db(Self::MAX_LINEAR_DB_SCALE as f32) {
            let db_vol = norm_vol as i64 * (db_max - db_min).0 + db_min.0 as i64;
            return self.db_to_raw(MilliBel(db_vol));
        }

        let mut norm_vol = norm_vol;

        if norm_vol == 0.0 {
            return Some(raw_min);
        }

        if db_min.0 != Self::SND_CTL_TLV_DB_GAIN_MUTE {
            let norm_min = 10.0_f64.powf((db_min - db_max).0 as f64 / 6000.0);
            norm_vol = norm_vol * (1.0 - norm_min) + norm_min;
        }

        let db_vol = (6000.0 * norm_vol.log10()) as i64 + db_max.0;
        self.db_to_raw(MilliBel(db_vol))
    }

    #[allow(dead_code)]
    fn get_volume(&mut self) -> Option<f64> {
        let raw_vol = self.get_volume_raw();
        self.raw_to_norm(raw_vol)
    }

    fn set_volume_raw(&mut self, vol: i64) {
        match self.dir {
            MixerDirection::Playback => self.selem.set_playback_volume_all(vol),
            MixerDirection::Capture => self.selem.set_capture_volume_all(vol),
        }
        .unwrap();

        if self.monitor {
            self.last_volume = Some(vol);
        }
    }

    fn set_volume(&mut self, norm_vol: f64) -> Option<()> {
        let raw_vol = self.norm_to_raw(norm_vol)?;
        self.set_volume_raw(raw_vol);
        Some(())
    }

    fn volume_has_update(&mut self) -> Option<i64> {
        if !self.monitor {
            return None;
        }

        if let Some(vol) = self.last_volume {
            let new_vol = self.get_volume_raw();
            if vol == new_vol {
                return None;
            }
            return Some(new_vol);
        }

        let new_vol = self.get_volume_raw();
        Some(new_vol)
    }
}

#[derive(Debug)]
struct MixerControlDesc<'a> {
    card_name: &'a str,
    name: &'a str,
    index: u32,
    dir: MixerDirection,
    monitor: bool,
    scale: f64,
    volume_mapper: MixerVolumeMapper,
}

struct MixerSyncGroup<'a> {
    controls: &'a mut [MixerControl<'a>],
}

impl<'a> MixerSyncGroup<'a> {
    fn new(controls: &'a mut [MixerControl<'a>]) -> Self {
        Self { controls }
    }

    fn update(&mut self) {
        let mut has_update = false;

        let (norm_vol, other_controls): (Vec<f64>, Vec<&mut MixerControl>) = self
            .controls
            .iter_mut()
            .partition_map(|control| match control.volume_has_update() {
                Some(vol) if !has_update => {
                    has_update = true;
                    Either::Left(
                        control
                            .raw_to_norm(vol)
                            .expect("failed to convert to normalized volume"),
                    )
                }
                _ => Either::Right(control),
            });

        if let Some(norm_vol) = norm_vol.iter().next() {
            let norm_vol = *norm_vol;
            println!("Volume = {}", norm_vol);

            for control in other_controls {
                control.set_volume(norm_vol).expect("failed to set volume");
            }
        }
    }
}

fn main() {
    let control_descs = &[
        MixerControlDesc {
            card_name: "hw:Audio",
            name: "Main",
            index: 0,
            dir: MixerDirection::Playback,
            monitor: true,
            scale: 0.5,
            volume_mapper: MixerVolumeMapper::Auto,
        },
        MixerControlDesc {
            card_name: "hw:UAC2Gadget",
            name: "PCM",
            index: 0,
            dir: MixerDirection::Capture,
            monitor: true,
            scale: 1.0,
            volume_mapper: MixerVolumeMapper::Auto,
        },
    ];

    let card_names: Vec<&str> = control_descs.iter().map(|c| c.card_name).unique().collect();

    let mixers: HashMap<&str, alsa::Mixer> = card_names
        .iter()
        .map(|card_name| (*card_name, alsa::Mixer::new(card_name, false).unwrap()))
        .collect();

    let mut controls: Vec<MixerControl> = control_descs
        .iter()
        .map(|c| {
            MixerControl::new(
                mixers.get(c.card_name).unwrap(),
                c.name,
                c.index,
                c.dir,
                c.monitor,
                c.scale,
                c.volume_mapper,
            )
        })
        .collect();

    let mut sync_group = MixerSyncGroup::new(controls.as_mut_slice());

    loop {
        let mixers_vec: Vec<&alsa::Mixer> = mixers.values().collect();
        let descs: Vec<&dyn alsa::poll::Descriptors> = mixers_vec
            .iter()
            .map(|m| *m as &dyn alsa::poll::Descriptors)
            .collect();

        alsa::poll::poll_all(descs.as_slice(), -1).expect("failed to poll mixers");

        for mixer in mixers.values() {
            mixer.handle_events().unwrap();
        }
        sync_group.update();
    }
}
