#ifndef LLVM_MC_MCCODEEMITTER_H
#define LLVM_MC_MCCODEEMITTER_H
#include "llvm/ADT/APFloat.h"

namespace llvm {

class MCFixup;
class MCInst;
class MCSubtargetInfo;
class raw_ostream;
template<typename T> class SmallVectorImpl;

/// MCCodeEmitter - Generic instruction encoding interface.
class MCCodeEmitter {
protected: // Can only create subclasses.
  MCCodeEmitter();

public:
  MCCodeEmitter(const MCCodeEmitter &) = delete;
  MCCodeEmitter &operator=(const MCCodeEmitter &) = delete;
  virtual ~MCCodeEmitter();

  /// Lifetime management
  virtual void reset() {}

  /// Emit the prefixes of given instruction on the output stream.
  ///
  /// \param Inst a single low-level machine instruction.
  /// \param OS output stream.
  virtual void emitPrefix(const MCInst &Inst, raw_ostream &OS,
                          const MCSubtargetInfo &STI) const {}
  /// EncodeInstruction - Encode the given \p Inst to bytes on the output
  /// stream \p OS.
  virtual void encodeInstruction(const MCInst &Inst, raw_ostream &OS,
                                 SmallVectorImpl<MCFixup> &Fixups,
                                 const MCSubtargetInfo &STI) const = 0;
};

} // end namespace llvm

#endif // LLVM_MC_MCCODEEMITTER_H
