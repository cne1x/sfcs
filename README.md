# Space-Filling Curves in Scala

This began as a pure-Scala implementation of the Compact Hilbert Curve,
and is growing into a thoughts-in-code project representing how space-filling
curves in general ought to behave and be composed.

## Curves included

These are the space-filling curves that have already been encoded:

1.  rectilinear:  This is a naive curve roughly equivalent to row-major ordering.
1.  z-order:  This curve interleaves bits of the input ordinal numbers, which means that not all
    combinations of input precisions are valid.  For example, having a two-dimensional space in
    which the first dimension uses 10 bits and the second uses 2 bits is not a good fit for a
    z-order mapping.  (There are ways to overcome this, but for now it just complains.)
    [Geohashing](http://www.geohash.org) interleaves longitude and latitude ordinals via a z-order
    curve, so the implementation here should be considered a superset of that type of mapping.
1.  compact-hilbert:  Based on a "U"-shape path that recurses (while rotating, flipping), a
    Hilbert curve tends to have fewer, shorter discontinuities in the index-space than the z-order
    curve.  The "compact" refers to the fact that not all dimensions need share the same precision.

## Other resources

Papers concerning the compact Hilbert curve:

1.  Hamilton, C. H., Rau-Chaplin, A.
    "Compact Hilbert Indices for Multi-Dimensional Data".
    In Proceedings of the First International Conference on Complex, Intelligent and Software Intensive Systems
    published by the IEEE.  2007.
    [PDF](https://web.cs.dal.ca/~arc/publications/2-43/paper.pdf)
    
    1.  This contains the meat of the indexing algorithm used here, but does not contain the inverse.  
    
1.  C. Hamilton.  Compact Hilbert indices.  "Technical Report C-2006-07".
    Dalhousie University, Faculty of Computer Science, July 2006.
    [PDF](https://www.cs.dal.ca/sites/default/files/technical_reports/CS-2006-07.pdf)
    
    1.  This describes both the indexing method and the inverse index, but uses a slightly different
        form than the follow-up paper.

Other projects that may be of interest:

1.  the relatively new space-filling curve project ([code](https://github.com/geotrellis/curve.git), 
    [description](http://www.locationtech.org/proposals/sfcurve)) at LocationTech
1.  the [Uzaygezen](https://code.google.com/p/uzaygezen/) Java-based compact-Hilbert curve implementation
