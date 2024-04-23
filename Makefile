build:
	cargo build --release
	rm -f ./lua/crates_nvim.so
	mkdir -p ./lua
	cp ./target/release/libcrates_nvim.so ./lua/crates_nvim_lib.so
