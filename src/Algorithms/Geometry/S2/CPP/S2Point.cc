#include "s2.h"

extern "C" {
  void c_S2Point_sub (S2Point &r, S2Point &a1, S2Point &a2) { r = a1 - a2; }
  void c_S2Point_add (S2Point &r, S2Point &a1, S2Point &a2) { r = a1 + a2; }
  void c_S2Point_mul (S2Point &r, S2Point &a1, double &a2) { r = a1 * a2; }
  void c_S2Point_div (S2Point &r, S2Point &a1, double &a2) { r = a1 / a2; }
  void c_S2Point_neg (S2Point &r, S2Point &a1) { r = -a1; }
  void c_S2Point_angle (double &r, S2Point &a1, S2Point &a2) { r = a1.Angle (a2); }
  void c_S2Point_crossProd (S2Point &r, S2Point &a1, S2Point &a2) { r = a1.CrossProd (a2); }
  void c_S2Point_dotProd (double &r, S2Point &a1, S2Point &a2) { r = a1.DotProd (a2); }
  void c_S2Point_norm2 (double &r, S2Point &a1) { r = a1.Norm2 (); }
  void c_S2Point_norm (double &r, S2Point &a1) { r = a1.Norm (); }
  void c_S2Point_normalise (S2Point &r, S2Point &a1) { r = a1.Normalize (); }
  void c_S2Point_fabs (S2Point &r, S2Point &a1) { r = a1.Fabs (); }
  void c_S2Point_largestAbsComponent (long &r, S2Point &a1) { r = a1.LargestAbsComponent (); }
  void c_S2Point_ortho (S2Point &r, S2Point &a1) { r = a1.Ortho (); }
}
