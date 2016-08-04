#define private public
#include "s2cellid.h"

void getBits(int k, int &bits, uint32 *n, int i, int j);

extern "C" {
  void c_S2CellId_lsb (uint64 &r, S2CellId &a1) { r = a1.lsb (); }
  void c_S2CellId_lsbForLevel (uint64 &r, int &a1) { r = S2CellId::lsb_for_level (a1); }
  void c_S2CellId_parent (S2CellId &r, S2CellId &a1) { r = a1.parent (); }
  void c_S2CellId_parentForLevel (S2CellId &r, S2CellId &a1, int &a2) { r = a1.parent (a2); }
  void c_S2CellId_fromFacePosLevel (S2CellId &r, int &a1, uint64 &a2, int &a3) { r = S2CellId::FromFacePosLevel (a1, a2, a3); }
  void c_S2CellId_fromFaceIJ (S2CellId &r, int &a1, int &a2, int &a3) { r = S2CellId::FromFaceIJ (a1, a2, a3); }
  void c_S2CellId_fromPoint (S2CellId &r, S2Point &a1) { r = S2CellId::FromPoint (a1); }
  void c_S2CellId_fromLatLng (S2CellId &r, S2LatLng &a1) { r = S2CellId::FromLatLng (a1); }

  uint64 optc_S2CellId_fromFaceIJ (int a1, int a2, int a3) { return S2CellId::FromFaceIJ (a1, a2, a3).id (); }
}
