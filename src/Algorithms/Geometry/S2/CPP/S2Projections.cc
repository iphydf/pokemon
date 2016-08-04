#include "s2.h"

struct R2Vector
{
  double x, y;

  friend std::ostream &operator << (std::ostream &os, R2Vector const &a) {
    return os << "R2Vector { " << a.x << ", " << a.y << " }";
  }
};

template<typename T>
struct Maybe
{
  T value;
  int isJust;

  T *operator -> () { return &value; }

  friend std::ostream &operator << (std::ostream &os, Maybe const &a) {
    if (a.isJust)
      return os << "Just " << a.value;
    else
      return os << "Nothing";
  }
};

extern "C" {
  void c_S2Projections_projection (long &r) { r = S2_PROJECTION; }
  void c_S2Projections_maxEdgeAspect (double &r) { r = S2::kMaxEdgeAspect; }
  void c_S2Projections_maxDiagAspect (double &r) { r = S2::kMaxDiagAspect; }
  void c_S2Projections_stToUV (double &r, double &a1) { r = S2::STtoUV (a1); }
  void c_S2Projections_uvToST (double &r, double &a1) { r = S2::UVtoST (a1); }
  void c_S2Projections_faceUvToXyz (S2Point &r, double &a1, double &a2, int &a3) { r = S2::FaceUVtoXYZ (a3, a1, a2); }
  void c_S2Projections_faceXyzToUv (Maybe<R2Vector> &r, S2Point &a1, int &a2) { r.isJust = S2::FaceXYZtoUV (a2, a1, &r->x, &r->y); }
  void c_S2Projections_xyzToFace (int &r, S2Point &a1) { double u, v; r = S2::XYZtoFaceUV (a1, &u, &v); }
  void c_S2Projections_getUNorm (S2Point &r, double &a1, int &a2) { r = S2::GetUNorm (a2, a1); }
  void c_S2Projections_getVNorm (S2Point &r, double &a1, int &a2) { r = S2::GetVNorm (a2, a1); }
  void c_S2Projections_getNorm (S2Point &r, int &a1) { r = S2::GetNorm (a1); }
  void c_S2Projections_getUAxis (S2Point &r, int &a1) { r = S2::GetUAxis (a1); }
  void c_S2Projections_getVAxis (S2Point &r, int &a1) { r = S2::GetVAxis (a1); }
}
