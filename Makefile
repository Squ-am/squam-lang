.PHONY: build release install clean all-platforms

# Default target
build:
	cargo build -p squam

# Release build (optimized, current platform)
release:
	cargo build --release -p squam

# Cross-compile for all platforms
all-platforms:
	@mkdir -p dist
	@echo "Building for Linux x86_64..."
	cargo zigbuild --release -p squam --target x86_64-unknown-linux-gnu
	@cp target/x86_64-unknown-linux-gnu/release/squam dist/squam-linux-x86_64
	@echo "Building for Linux arm64..."
	cargo zigbuild --release -p squam --target aarch64-unknown-linux-gnu
	@cp target/aarch64-unknown-linux-gnu/release/squam dist/squam-linux-arm64
	@echo "Building for macOS x86_64..."
	cargo zigbuild --release -p squam --target x86_64-apple-darwin
	@cp target/x86_64-apple-darwin/release/squam dist/squam-macos-x86_64
	@echo "Building for macOS arm64..."
	cargo zigbuild --release -p squam --target aarch64-apple-darwin
	@cp target/aarch64-apple-darwin/release/squam dist/squam-macos-arm64
	@echo "Building for Windows x86_64 (Docker)..."
	docker run --rm -v $(PWD):/project -w /project rust:latest bash -c \
		"apt-get update -qq && apt-get install -qq -y mingw-w64 > /dev/null && \
		rustup target add x86_64-pc-windows-gnu > /dev/null 2>&1 && \
		cargo build --release -p squam --target x86_64-pc-windows-gnu"
	@cp target/x86_64-pc-windows-gnu/release/squam.exe dist/squam-windows-x86_64.exe
	@echo ""
	@echo "All binaries in dist/:"
	@ls -lh dist/

# Individual platform targets
linux-x86_64:
	cargo zigbuild --release -p squam --target x86_64-unknown-linux-gnu

linux-arm64:
	cargo zigbuild --release -p squam --target aarch64-unknown-linux-gnu

macos-x86_64:
	cargo zigbuild --release -p squam --target x86_64-apple-darwin

macos-arm64:
	cargo zigbuild --release -p squam --target aarch64-apple-darwin

windows:
	docker run --rm -v $(PWD):/project -w /project rust:latest bash -c \
		"apt-get update -qq && apt-get install -qq -y mingw-w64 > /dev/null && \
		rustup target add x86_64-pc-windows-gnu > /dev/null 2>&1 && \
		cargo build --release -p squam --target x86_64-pc-windows-gnu"

# Install to ~/.local/bin
INSTALL_DIR ?= $(HOME)/.local/bin

install: release
	@mkdir -p $(INSTALL_DIR)
	@cp target/release/squam $(INSTALL_DIR)/squam
	@echo "Installed squam to $(INSTALL_DIR)/squam"

# Install globally
install-global: release
	@sudo cp target/release/squam /usr/local/bin/squam
	@echo "Installed squam to /usr/local/bin/squam"

# Copy binaries to web for distribution
dist-web: all-platforms
	@mkdir -p web/static/releases
	@cp dist/* web/static/releases/
	@cp target/x86_64-pc-windows-gnu/release/squam.exe web/static/releases/squam-windows-x86_64.exe 2>/dev/null || true
	@echo "Binaries copied to web/static/releases/"
	@ls -lh web/static/releases/

# Clean build artifacts
clean:
	cargo clean
	rm -rf dist
	rm -rf web/static/releases
