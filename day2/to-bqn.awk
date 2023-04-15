BEGIN {
    IF=" "
    print "⟨";
}

/.*/ {
    print "⟨'" $1 "','" $2 "'⟩";
}

END {
    print "⟩";
}
