#!/bin/bash

set -ex
export TOOLS=/home/vagrant/tools
export RISCV=${TOOLS}/riscv
export PATH=${RISCV}/bin:${PATH}
mkdir -p /home/vagrant/tools

cd /home/vagrant/tools
wget -q https://github.com/riscv-collab/riscv-gnu-toolchain/releases/download/2021.09.21/riscv64-elf-ubuntu-20.04-nightly-2021.09.21-nightly.tar.gz
tar -xf riscv64-elf-ubuntu-20.04-nightly-2021.09.21-nightly.tar.gz

