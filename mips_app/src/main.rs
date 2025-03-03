use mips_lib::Context;

mod window;
mod app;
mod error;

fn main() {
    match app::App::start() {
        Ok(app) => {
            //app.insert_disc(Path::new());

        },
        Err(e) => {
            panic!("The app failed: {:?}", e);
        }
    }
}