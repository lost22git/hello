#!/usr/bin/env -S julia -t auto

@show Threads.nthreadpools()
@show Threads.nthreads()
@show Threads.nthreads(:default)
@show Threads.nthreads(:interactive)

@time let count = Threads.Atomic{Int}(0)
    Threads.@threads for i in 1:10000000
        Threads.atomic_add!(count, 1)
    end
    @show count[]
end

# @threads
# just for for-loop
@time Threads.@threads for i in 1:10
    sleep(1)
end

# @spawn
# yield task to threadpool
@time @sync for i in 1:10
    Threads.@spawn sleep(1)
end

# @async
# yield task to current thread
@time @sync for i in 1:10
    @async sleep(1)
end
