function logistic_fixed_points(r_lower, r_upper, r_step, iterations, tail, tolerance)
    points = []
    
    for r in r_lower:r_step:r_upper
        orbit = let v = zeros(iterations + 1); v[1] = 0.5; v end
        
        for i in 2:length(orbit)
            previous = orbit[i - 1]
            orbit[i] = r * previous * (1 - previous)
        end
        
        tail_samples = orbit[end-tail+1:end]
        quantised = round.(tail_samples ./ tolerance) .* tolerance
        fixed_points = unique(quantised)

        for p in fixed_points
            push!(points, (r, p))
        end
        
    end

    return points
end

# the_points = logistic_fixed_points(3.56, 4, 0.0001, 2000, 1500, 0.0001)
