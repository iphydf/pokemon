#include "s1angle.h"

extern "C" {
  void c_S1Angle_fromDegrees (S1Angle &r, double  &a1) { r = S1Angle::Degrees (a1); }
  void c_S1Angle_fromRadians (S1Angle &r, double  &a1) { r = S1Angle::Radians (a1); }
  void c_S1Angle_fromE5      (S1Angle &r, int32   &a1) { r = S1Angle::E5 (a1); }
  void c_S1Angle_fromE6      (S1Angle &r, int32   &a1) { r = S1Angle::E6 (a1); }
  void c_S1Angle_fromE7      (S1Angle &r, int32   &a1) { r = S1Angle::E7 (a1); }
  void c_S1Angle_degrees     (double  &r, S1Angle &a1) { r = a1.degrees (); }
  void c_S1Angle_radians     (double  &r, S1Angle &a1) { r = a1.radians (); }
  void c_S1Angle_between     (S1Angle &r, S2Point const &a1, S2Point const &a2) { r = S1Angle (a1, a2); }
}
