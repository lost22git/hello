#!/usr/bin/env -S DEBUG=1 crun

# ---
# fancyline:
#   github: Papierkorb/fancyline
#   version: ~> 0.4.1
# ...

require "fancyline"

fancy = Fancyline.new
puts "Press Ctrl-C or Ctrl-D to quit."

# Add a display middleware to add highlighting.  Make sure that you don't modify
# the visual length of *line*, else cursor won't match up with its real
# position.
fancy.display.add do |ctx, line, yielder|
  # We underline command names
  line = line.gsub(/^\w+/, &.colorize.mode(:underline))
  line = line.gsub(/(\|\s*)(\w+)/) do
    "#{$1}#{$2.colorize.mode(:underline)}"
  end

  # And turn --arguments green
  line = line.gsub(/--?\w+/, &.colorize(:green))

  # Then we call the next middleware with the modified line
  yielder.call ctx, line
end

def get_command(ctx)
  line = ctx.editor.line
  cursor = ctx.editor.cursor.clamp(0, line.size - 1)
  pipe = line.rindex('|', cursor)
  line = line[(pipe + 1)..-1] if pipe

  line.split.first?
end

fancy.actions.set Fancyline::Key::Control::CtrlO do |ctx|
  if command = get_command(ctx) # Figure out the current command
    system("man #{command}")    # And open the man-page of it
  end
end

fancy.autocomplete.add do |ctx, range, word, yielder|
  completions = yielder.call(ctx, range, word)

  # The `word` may not suffice for us here.  It'd be fine however for command
  # name completion.

  # Find the range of the current path name near the cursor.
  prev_char = ctx.editor.line[ctx.editor.cursor - 1]?
  if !word.empty? || prev_char.in?('/', '.')
    # Then we try to find where it begins and ends
    arg_begin = ctx.editor.line.rindex(' ', ctx.editor.cursor - 1) || 0
    arg_end = ctx.editor.line.index(' ', arg_begin + 1) || ctx.editor.line.size
    range = (arg_begin + 1)...arg_end

    # And using that range we just built, we can find the path the user entered
    path = ctx.editor.line[range].strip
  end

  # Find suggestions and append them to the completions array.
  Dir["#{path}*"].each do |suggestion|
    base = File.basename(suggestion)
    suggestion += '/' if Dir.exists? suggestion
    completions << Fancyline::Completion.new(range, suggestion, base)
  end

  completions
end

# Custom widget which upon start presents the user with a list of faces to
# choose from, which are then pasted at the cursor position.
class FacesWidget < Fancyline::Widget
  FACES = ["😁", "😭", "🙃"]
  @sub_info_handle = Cute::ConnectionHandle.new(0)

  def start(ctx)
    # Add a sub_info middleware to show the user options to choose from.
    # Store the handle so we can remove it later again in `#stop`.
    @sub_info_handle = ctx.fancyline.sub_info.add do |ctx, yielder|
      lines = yielder.call(ctx)

      options = FACES.map_with_index { |str, idx| "#{idx}: #{str}" }
      lines << "  " + options.join("  ")

      lines
    end
  end

  def stop(ctx)
    # On stop, remove our middleware and force a redraw later on.
    ctx.fancyline.sub_info.disconnect @sub_info_handle
    ctx.clear_info
  end

  def handle(ctx, char : Char) : Bool
    # The user sent us some input
    if choose = char.to_i?     # Try to read it as numeric
      if face = FACES[choose]? # Is there a matching face?
        # Apply a completion to the line buffer at the cursor position.
        range = ctx.editor.cursor...ctx.editor.cursor
        ctx.editor.apply Fancyline::Completion.new(range, face)
      end

      # Remove this widget now and tell the `Context` that we handled this
      # input.
      ctx.stop_widget
      true
    else
      # If it's not some numeric, stop this widget and proceed as usual.
      super
    end
  end
end

# Add a key binding to allow the user to launch our widget at any time.
fancy.actions.set Fancyline::Key::Control::CtrlF do |ctx|
  ctx.start_widget FacesWidget.new
end

begin
  while input = fancy.readline("$ ")
    system(input)
  end
rescue err : Fancyline::Interrupt
  puts "Bye."
end
