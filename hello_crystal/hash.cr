# Hash
#
# def find_by_key(key)
#   if size <= 8 # small entries, no need indices
#     return liner_scan_entries_to_match(key).value
#   else
#     hash = key.hash
#     indices_index = hash mod indices.size
#     loop do
#        entries_index = indices[indices_index]
#        found = entries[entries_index]
#        return found.value if found.hash = hash && found.key == key
#        indices_index += 1 # open addressing: find next slot
#      end
#   end
# end

class Cat
  property name : String

  def initialize(@name); end

  def hash
    name.hash
  end
end

require "spec"
describe Hash do
  it "change Cat's hash, we still can find it from Hash" do
    map = {} of Cat => String

    kitty_cat = Cat.new("kitty")
    map[kitty_cat] = "kitty's color is white"
    tom_cat = Cat.new("tom")
    map[tom_cat] = "tom's color is blue"

    map[kitty_cat]?.should_not be_nil
    # change Cat's hash by change name
    kitty_cat.name = "kitty cat"
    map[kitty_cat]?.should_not be_nil
  end

  it "change Cat's hash, we cannot find it from Hash (size > 8)" do
    map = {} of Cat => String

    kitty_cat = Cat.new("kitty")
    map[kitty_cat] = "kitty's color is white"
    # now map's size > 8
    (1..8).each do |i|
      map[Cat.new("tom-#{i}")] = "tom-#{i}'s color is blue"
    end

    map[kitty_cat]?.should_not be_nil
    # change Cat's hash by change name
    kitty_cat.name = "kitty cat"
    map[kitty_cat]?.should be_nil
  end
end
