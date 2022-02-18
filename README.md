# gmShiny

Shiny App Interface for geomorph

See Baken et al 2021 for information on the shiny app introduction. This app is recommended for use of small to moderate datasets. For more complex analyses or analysis of large datasets, please use geomorph directly in R or R Studio.

Learn about the various options for launching the app at www.gmshiny.com.


# Changes to app: v0.1.0
1) Download button now pulls a zip file of coordinates and the current state of the data (no longer two steps)
2) Unlimited idle time for session
3) Bug fix: warp grid point selected tracks axis flips and changes in which pc is shown on a particular axis
4) Bug fixes on Symmetry page: 
    - Previously: 'use symmetric components' was passing those landmarks back to the pre-gpa step, thus those lms were put through gpa again erroneously
    - Export symmetric and asymmetric components of shape now functional
5) 'Clear Symmetry Settings' button added
6) Added demo files to guide formatting for imported data, found in Extras page associated with the relevant Tutorial video
7) Hid 'Define Links and Semilandmarks' tab when 3D data are uploaded. Future work: accommodating semi landmark definition for 3D data.
8) Various updates to instructions
9) Various other visualization bugs fixed
10) Hosted on a new server (Chatham University)
11) R updated to v4.0.4
