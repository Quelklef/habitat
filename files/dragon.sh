dgn_loc=/per/dgn

function dgn.up {(
  set -euo pipefail
  { mount -l | grep -q "$dgn_loc"; } && { echo >&2 "Already mounted!"; return 1; }
  [ -e $dgn_loc ] && { ls -1qaA "$dgn_loc" | grep -q .; } \
    && { echo >&2 "Refusing to overwrite nonempty $dgn_loc"; return 1; }
  sudo mkdir -p $dgn_loc
  sudo chown "$USER":users $dgn_loc
  sshfs \
    -o auto_cache,no_readahead,Compression=yes,allow_root \
    -p23 u309918@u309918.your-storagebox.de:/home $dgn_loc
  echo ok
)}

function dgn.down {(
  set -euo pipefail
  { mount -l | grep -q "$dgn_loc"; } || { echo >&2 "Already down"; return 1; }
  fusermount -u $dgn_loc
  sudo rm -r $dgn_loc
  echo ok
)}
