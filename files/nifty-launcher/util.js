const proc = require('child_process');
const plib = require('path');
const fs = require('fs');

exports.capitalize =
function(str) {
  return str.slice(0, 1).toUpperCase() + str.slice(1);
}


/*

Execute a program or bash command

Four ways to call it:

  // Run a bash command and detach it
  exec('google-chrome 2>&1 1>$HOME/chrome-log');

  // Run a bash command and capture stdout as a string
  const stdout = exec('echo test', { capture: true })

  // Execute a utility and detach it
  exec(['/usr/bin/alacritty', '--config-file', '/home/me/alacritty.yaml'])

  // Execute a utility and capture stdout as a string
  const stdout = exec(
    ['/usr/bin/env', 'bash', '-c', 'echo test'],
    { capture: true },
  )

*/
const exec =
exports.exec =
function(theseArg, theseOpts = {}) {

  const thoseArgs = (
    typeof theseArg === 'string'
      ? ['/usr/bin/env', ['bash', '-c', theseArg]]
      : [...theseArg.slice(0, 1), theseArg.slice(1)]
  );

  if (theseOpts.capture) {
    return proc.spawnSync(...thoseArgs).stdout.toString();
  } else {
    proc.spawn(
      ...thoseArgs,
      {
        detached: true,
        stdio: ['ignore', 'inherit', 'inherit'],
      }
    ).unref();
  }

};


/* Escape a string into Bash syntax */
exports.escapeForBash =
function(str) {
  return "'" + str.replace(/'/g, "'\\''") + "'";
};


/* Use the NWjs API to copy some text to the system clipboard */
exports.copyToClipboard =
function(text) {
  nw.Clipboard.get().set(text);
};


/* Turn a local path into a file URI */
exports.mkIcon =
function(fname) {
  return 'file://' + plib.resolve(__dirname, fname);
};


/* Yield all binaries in PATH as { name, path } pairs */
exports.getPATH =
function*() {
  let PATH = exec('echo "$PATH"', { capture: true }).split(':');
  PATH = [...new Set(PATH)];  // deduplicate
  for (const bin of PATH) {
    if (!fs.existsSync(bin)) continue;
    for (const name of fs.readdirSync(bin)) {
      const path = plib.resolve(bin, name);
      yield { name, path: path };
    }
  }
};
