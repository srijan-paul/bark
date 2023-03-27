#!/bin/bash
rm -rf docs
cd site
bark build
mv build ../docs
