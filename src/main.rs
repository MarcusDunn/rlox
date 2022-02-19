use std::error::Error;
use std::fs::File;
use std::io::Read;

use depends::lib::runner::{eval, run};

fn main() -> Result<(), Box<dyn Error>> {
    let args = std::env::args();
    match &args.collect::<Vec<_>>()[..] {
        [_] => match run() {
            Ok(_) => Ok(()),
            Err(err) => Err(err),
        },
        [_, file_name] => {
            let mut str = String::new();
            let mut f = File::open(file_name)?;
            f.read_to_string(&mut str)?;
            eval(str);
            Ok(())
        }
        _ => {
            print_usage();
            Ok(())
        }
    }
}

fn print_usage() {
    print!(
        "
LOX:
    usage:
        - repl: lox
        - file: lox <filename>
        - help: lox -- help
"
    )
}
