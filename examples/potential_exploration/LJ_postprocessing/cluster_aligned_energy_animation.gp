# --- GLOBAL PATH CONFIG ---
path     = '/home/dph/nested_fit/test_LJ/cluster/downloads/nested_runs/2D/classical'
list_N   = "2 3 4 5 6 7 8 9 10 11 12 13"
list_N   = "6"
K        = "16000"
n_frames = 200

# --- DATA PARSING RULES ---
set datafile commentschars "#"
set datafile separator whitespace
set datafile missing NaN

do for [N_str in list_N] {
    # Choose file with atom coordinates
    # file = path . sprintf('/N%s/K%s/nf_output_points.txt', N_str, K)
    # file = path . sprintf('/N%s/K%s/cluster_points_aligned.txt', N_str, K)
    file = path . sprintf('/N%s/K%s/cluster_points_aligned_pca.txt', N_str, K)
    # file = path . sprintf('/N%s/K%s/cluster_points_pca_only.txt', N_str, K)

    # Energy file
    energy_file = path . sprintf('/N%s/K%s/nf_output_energy.txt', N_str, K)

    if (system(sprintf("test -f %s && echo 1 || echo 0", file)) == 0) {
        print sprintf("WARNING: File not found for N=%s. Skipping...", N_str)
        continue
    }

    set terminal gif animate delay 10 loop 0 size 800,800
    set output sprintf('cluster_aligned_energy_N%s.gif', N_str)
    
    set xrange [-0.5:0.5]; set yrange [-0.5:0.5]
    set size square
    set key outside
    
    num_atoms  = int(N_str)
    line_count = int(system(sprintf("wc -l < %s", file)))
    step_val   = (line_count <= n_frames) ? 1 : int(line_count / n_frames)

    print sprintf(">>> Processing N=%d (%d rows, step %d)", num_atoms, line_count, step_val)

    # --- Create temporary sub-sampled files (points + energy in sync) ---
    temp_file   = "temp_subset.tmp"
    temp_energy = "temp_energy.tmp"
    system(sprintf("awk 'NR %% %d == 0' %s > %s", step_val, file, temp_file))
    system(sprintf("awk 'NR %% %d == 0' %s > %s", step_val, energy_file, temp_energy))

    actual_line_count = int(system(sprintf("wc -l < %s", temp_file)))

    frame = 0
    do for [row=0:actual_line_count-1] {

        # Energy is in the second column of nf_output_energy.txt
        energy_val = system(sprintf("awk 'NR == %d {print $2}' %s", row + 1, temp_energy))

        frame = frame + 1
        set title sprintf('N=%d | frame %d | E=%s', num_atoms, frame, energy_val)

        # Shift atoms so COM is at 0.5, 0.5
        plot for [i=1:num_atoms] \
                for [j=-0:0] \
                for [k=-0:0] \
            temp_file every ::row::row \
            using (column(3+2*i-1) + j):(column(3+2*i) + k) \
            with points pt 7 ps 1.2 \
            lc rgb ( (j==0 && k==0) ? "red" : "gray" ) \
            notitle   
    }

    system(sprintf("rm %s %s", temp_file, temp_energy))
    set output 
    print sprintf("<<< Finished N=%s", N_str)
}