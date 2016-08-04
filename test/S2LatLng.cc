#include "s2latlng.h"

extern "C" {
  void c_S2LatLng_fromRadians (S2LatLng &r, double &a1, double &a2) { r = S2LatLng::FromRadians (a1, a2); }
  void c_S2LatLng_fromDegrees (S2LatLng &r, double &a1, double &a2) { r = S2LatLng::FromDegrees (a1, a2); }
  void c_S2LatLng_fromLatLng (S2LatLng &r, S1Angle &a1, S1Angle &a2) { r = S2LatLng (a1, a2); }
  void c_S2LatLng_fromPoint (S2LatLng &r, S2Point &a1) { r = S2LatLng (a1); }
  void c_S2LatLng_fromE5 (S2LatLng &r, int32 &a1, int32 &a2) { r = S2LatLng::FromE5 (a1, a2); }
  void c_S2LatLng_fromE6 (S2LatLng &r, int32 &a1, int32 &a2) { r = S2LatLng::FromE6 (a1, a2); }
  void c_S2LatLng_fromE7 (S2LatLng &r, int32 &a1, int32 &a2) { r = S2LatLng::FromE7 (a1, a2); }
  void c_S2LatLng_latitude (S1Angle &r, S2Point &a1) { r = S2LatLng::Latitude (a1); }
  void c_S2LatLng_longitude (S1Angle &r, S2Point &a1) { r = S2LatLng::Longitude (a1); }
  void c_S2LatLng_lat (S1Angle &r, S2LatLng &a1) { r = a1.lat (); }
  void c_S2LatLng_lng (S1Angle &r, S2LatLng &a1) { r = a1.lng (); }
  void c_S2LatLng_normalised (S2LatLng &r, S2LatLng &a1) { r = a1.Normalized (); }
  void c_S2LatLng_toPoint (S2Point &r, S2LatLng &a1) { r = a1.ToPoint (); }
  void c_S2LatLng_getDistance (S1Angle &r, S2LatLng &a1, S2LatLng &a2) { r = a1.GetDistance (a2); }
  void c_S2LatLng_add (S2LatLng &r, S2LatLng &a1, S2LatLng &a2) { r = a1 + a2; }
  void c_S2LatLng_sub (S2LatLng &r, S2LatLng &a1, S2LatLng &a2) { r = a1 - a2; }
  void c_S2LatLng_mul (S2LatLng &r, S2LatLng &a1, double &a2) { r = a1 * a2; }
}
