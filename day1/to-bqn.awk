BEGIN {
    print "⟨";
    print "⟨";
}

/^$/ {
    print "⟩,⟨";    
}

/[0-9]+/ {
    print $0 ",";
}

END {
    print "⟩";
    print "⟩";
}
