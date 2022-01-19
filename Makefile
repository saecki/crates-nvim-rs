build:
	cargo build --release
	rm -f ./lua/libcrates_nvim.so
	mkdir -p ./lua
	cp ./target/release/libcrates_nvim.so ./lua/libcrates_nvim.so
	mkdir -p ./lua/deps/
	cp ./target/release/deps/*.rlib ./lua/deps/

