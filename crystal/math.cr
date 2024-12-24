# !/usr/bin/env crystal

require "spec"

describe "Test" do
  it "div / mod" do
    (3 / 2).should eq 1.5

    # div floor
    (3 // 2).should eq 1
    (-3 // -2).should eq 1
    (-3 // 2).should eq -2
    (3 // -2).should eq -2

    (3 % 2).should eq 1
    (-3 % -2).should eq -1
    (-3 % 2).should eq 1
    (3 % -2).should eq -1

    3.divmod(2).should eq({1, 1})
  end

  it "overflow" do
    expect_raises(OverflowError) do
      255_u8 + 1_u8
    end
    (255_u8 &+ 1_u8).should eq 0

    expect_raises(OverflowError) do
      127_i8 + 1_i8
    end
    (127_i8 &+ 1_i8).should eq -128_i8
  end
end
