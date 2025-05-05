use std::sync::mpsc;
use std::thread;

pub mod spu;
mod reverb_resampler;
mod fir;
mod fifo;

pub struct Handle {
    handle: Option<thread::JoinHandle<()>>,
    audio_sender: mpsc::Sender<Vec<i16>>,
    audio_receiver: mpsc::Receiver<Vec<i16>>,
}

