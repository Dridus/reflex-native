platforms = host ios android
cabal_files = $(shell find . -type f -a -name '*.cabal' | grep -v '^[.]/_build' | grep -v '^[.]/[.]')
nix_files = default.nix $(shell find . -type f -a -name default.nix | grep -v '^[.]/_build' | grep -v '^[.]/[.]')
bash = $(shell nix-instantiate --eval -E '(import <nixpkgs> {}).bash + /bin/bash')
libcxx_host = $(shell nix-instantiate --eval -E '"$${(import ./.).nixpkgs.libcxx}"')
libcxx_ios = $(shell nix-instantiate --eval -E '"$${(import ./.).iosArm64.libcxx}"')

.PHONY: all clean $(platforms)

# this sed hackery is here to work around a shortcoming with cabal new-build where error and warning messages get output with paths that are relative to the
# package being built, not the project root, and so vim (or similar) which try to parse those messages to allow quick navigation to the source line get
# bamboozled.

host: _build/host/shell host.project
	set -eo pipefail ; env -i $(bash) _build/host/shell cabal --project-file=host.project --builddir=_build/host/dist new-build kiwi-dsl 2>&1 | sed -e 's,^src/,kiwi/dsl/src/,g'
	set -eo pipefail ; env -i $(bash) _build/host/shell cabal --project-file=host.project --builddir=_build/host/dist new-build kiwi-cpp 2>&1 | sed -e 's,^kiwi/,kiwi/binding/cpp/kiwi/,g' -e 's,^src/,kiwi/binding/cpp/src/,g' -e 's,^cbits/,kiwi/binding/cpp/cbits/,g'
	set -eo pipefail ; env -i $(bash) _build/host/shell cabal --project-file=host.project --builddir=_build/host/dist new-test kiwi-cpp 2>&1 | sed -e 's,^test/,kiwi/binding/cpp/test/,g'
	set -eo pipefail ; env -i $(bash) _build/host/shell cabal --project-file=host.project --builddir=_build/host/dist new-build reflex-native 2>&1 | sed -e 's,^src/,reflex-native/src/,g'
	set -eo pipefail ; env -i $(bash) _build/host/shell cabal --project-file=host.project --builddir=_build/host/dist new-build reflex-native-test 2>&1 | sed -e 's,^src/,reflex-native-test/src/,g'
	set -eo pipefail ; env -i $(bash) _build/host/shell cabal --project-file=host.project --builddir=_build/host/dist new-test reflex-native-test 2>&1 | sed -e 's,^test/,reflex-native-test/test/,g'
	set -eo pipefail ; env -i $(bash) _build/host/shell cabal --project-file=host.project --builddir=_build/host/dist new-build reflex-native-draggy 2>&1 | sed -e 's,^src/,examples/draggy/src/,g'
	set -eo pipefail ; env -i $(bash) _build/host/shell cabal --project-file=host.project --builddir=_build/host/dist new-test reflex-native-draggy 2>&1 | sed -e 's,^test/,examples/draggy/test/,g'

ios: _build/ios/shell ios.project
	set -eo pipefail ; env -i $(bash) _build/ios/shell cabal --project-file=ios.project --builddir=_build/ios/dist new-build hs-kiwi 2>&1 | sed -e 's,^src/,hs-kiwi/src/,g'
	set -eo pipefail ; env -i $(bash) _build/ios/shell cabal --project-file=ios.project --builddir=_build/ios/dist new-build reflex-native 2>&1 | sed -e 's,^src/,reflex-native/src/,g'
	set -eo pipefail ; env -i $(bash) _build/ios/shell cabal --project-file=ios.project --builddir=_build/ios/dist new-build reflex-native-draggy 2>&1 | sed -e 's,^src/,examples/draggy/src/,g'

host.project: _build/host/shell host.project.template
	sed -e "s,@libcxx@,$(libcxx_host),g" < host.project.template > host.project

ios.project: _build/ios/shell ios.project.template
	sed -e "s,@libcxx@,$(libcxx_ios),g" < ios.project.template > ios.project

clean:
	rm -rf _build
	rm -f host.project ios.project android.project

all: $(platforms)

_build/%/shell: Makefile $(nix_files) $(cabal_files)
	mkdir -p $(dir $@)
	mkdir -p _build/$*/nix-root
	rm -f $@
	nix-shell --pure --add-root _build/$*/nix-root/nix-gc-root --indirect -A shells.$* --run 'declare -p | grep -v -E "^declare( -[^ ]* )?(BASH_[^=]*|BASHOPTS|BASHPID|EUID|GROUPS|PPID|SHELLOPTS|UID)="' > $@
	echo '"$$@"' >> $@
	echo 'exit $$?' >> $@
	chmod +x $@
