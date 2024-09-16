require "spec"

describe "Process" do
  it "`cmd`: inherit stderr and return stdout, $? is set to status" do
    # stderr: "sh: 1: InvalidCmd: not found"
    output = `InvalidCmd`
    output.should eq ""
    $?.success?.should be_false

    output = `crystal`
    output.should_not eq ""
    $?.success?.should be_true

    # stderr: "Error: unknown command: -invalid_param"
    output = `crystal -invalid_param`
    output.should eq ""
    $?.success?.should be_false

    # redirect stderr to stdout
    output = `InvalidCmd 2>&1`
    output.should_not eq ""
    $?.success?.should be_false
  end

  it "Process.run: run cmd and wait it complete" do
    expect_raises IO::Error do
      Process.run("InvalidCmd")
    end

    # ignore stdout and stderr by default, return status
    status = Process.run("crystal")
    unless status.success?
      p! status.exit_status
      p! status.exit_reason
      if status.normal_exit?
        p! status.exit_code
      elsif status.signal_exit?
        p! status.exit_signal
      else
      end
    end

    # pipe by default, block handle process
    block_result = Process.run("crystal") do |process|
      process.output.gets_to_end.should_not eq ""
      process.error.gets_to_end.should eq ""
      "hello"
    end
    block_result.should eq "hello"
  end

  it "Process.new: run cmd but do not wait" do
    expect_raises IO::Error do
      Process.new("InvalidCmd")
    end

    # ignore stdout and stderr by default
    process = Process.new("crystal")
    status = process.wait
    unless status.success?
      p! status.exit_status
      p! status.exit_reason
      if status.normal_exit?
        p! status.exit_code
      elsif status.signal_exit?
        p! status.exit_signal
      else
      end
    end

    # pipe stdout and stderror to current process
    process = Process.new("crystal", output: Process::Redirect::Pipe, error: Process::Redirect::Pipe)
    process.output.gets_to_end.should_not eq ""
    process.error.gets_to_end.should eq ""
    # wait for completed and close any pipe
    status = process.wait
    unless status.success?
      p! status.exit_status
      p! status.exit_reason
      if status.normal_exit?
        p! status.exit_code
      elsif status.signal_exit?
        p! status.exit_signal
      else
      end
    end
  end

  it "Process.exec: replaces the current process with a new one" do
  end
end
