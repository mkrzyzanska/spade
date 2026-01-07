#### Writing the hdr function


hdrhist <- function(hdr, z, pz){
    # Step 1: Compute bin edges and probabilities
    bin_start <- z[-length(z)]
    bin_end   <- z[-1]
    bin_prob  <- diff(pz)
    bin_width <- bin_end - bin_start
    bin_dens  <- bin_prob / bin_width  # height of each bar


    # Step 2: sort bin densities
    ord <- order(bin_dens, decreasing = TRUE)
    sorted_start <- bin_start[ord]
    sorted_end   <- bin_end[ord]
    sorted_dens  <- bin_dens[ord]
    sorted_width <- bin_width[ord]
    sorted_prob  <- bin_prob[ord]  # or: sorted_dens * sorted_width


    # Step 3: 

    target_prob <- hdr
    cum_prob <- cumsum(sorted_prob)

    # First index that exceeds the target
    cut_idx <- which(cum_prob >= target_prob)[1]

    # Fully include bins before that
    included_idx <- ord[1:(cut_idx - 1)]

    # Final partial bin
    partial_idx <- ord[cut_idx]
    remaining_mass <- target_prob - sum(bin_prob[included_idx])
    partial_density <- bin_dens[partial_idx]
    partial_width <- remaining_mass / partial_density

    # Compute edge of partial bin
    partial_start <- bin_start[partial_idx]
    partial_edge <- partial_start + partial_width

    # Fully included bins
    hdr_intervals <- data.frame(
      start = bin_start[included_idx],
      end   = bin_end[included_idx]
    )

    # Add partial bin
    partial_interval <- data.frame(
      start = partial_start,
      end   = partial_edge
    )

    # Combine and sort
    hdr_intervals <- rbind(hdr_intervals, partial_interval)
    hdr_intervals <- hdr_intervals[order(hdr_intervals$start), ]

    # Step 1: Sort intervals by start
    hdr_intervals <- hdr_intervals[order(hdr_intervals$start), ]

    # Step 2: Merge adjacent or overlapping intervals
    merged_intervals <- list()
    current_start <- hdr_intervals$start[1]
    current_end   <- hdr_intervals$end[1]

    for (i in 2:nrow(hdr_intervals)) {
      s <- hdr_intervals$start[i]
      e <- hdr_intervals$end[i]
  
    if (s <= current_end) {
        # Overlapping or adjacent — extend current interval
        current_end <- max(current_end, e)
      } else {
        # Disjoint — save current and start a new one
        merged_intervals[[length(merged_intervals) + 1]] <- c(current_start, current_end)
        current_start <- s
        current_end <- e
      }
    }

    # Add last interval
    merged_intervals[[length(merged_intervals) + 1]] <- c(current_start, current_end)

    # Convert to data.frame
    merged_df <- do.call(rbind, merged_intervals)
    colnames(merged_df) <- c("earlier", "later")
    merged_df <- as.data.frame(merged_df)
    my_list<-list()
    my_list$hdr <- merged_df

    # Final output
    print(my_list)
    return(my_list)
    
}