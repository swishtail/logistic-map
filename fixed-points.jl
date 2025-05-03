using CairoMakie

function logistic_fixed_points(r_lower, r_upper, r_step, iterations, tail, tolerance)
    points = []
    
    for r in r_lower:r_step:r_upper
        orbit = let v = zeros(iterations + 1); v[1] = 0.5; v end
        
        for i in 2:length(orbit)
            previous = orbit[i - 1]
            orbit[i] = r * previous * (1 - previous)
        end
        
        tail_values = orbit[end-tail+1:end]
        quantised = round.(tail_values ./ tolerance) .* tolerance
        fixed_points = unique(quantised)
        
        for p in fixed_points
            push!(points, (r, p))
        end   
    end
    return points
end

# For rendering with CairoMakie, best to set r-step and quantisation to twice desired resolution
# e.g. r-step    = (3.88 - 3.8)/8000 = 0.00001
#      tolerance = (0.6 - 0.4)/6000 ~= 0.00003
the_points = logistic_fixed_points(3.8, 3.88, 0.00001, 10000, 4000, 0.00003)
r_vals = first.(the_points)
x_vals = last.(the_points)

fig = Figure(backgroundcolor = :black)
ax = Axis(fig[1, 1], limits=(3.8, 3.88, 0.4, 0.6), width=4000, height=3000, backgroundcolor = :black)

scatter!(ax, r_vals, x_vals; color = :white, markersize = 1)
resize_to_layout!(fig)
# save("logistic_fixed_points.png", fig; px_per_unit = 1)
