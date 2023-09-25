
SIGDOC ?= ../sigdoc

BASIS ?= ~/gits/mlkit/basis

BASISFILES0 = GENERAL.sig General.sml OPTION.sig Option.sml LIST.sig List.sml LIST_PAIR.sml \
  ListPair.sml VECTOR.sig Vector.sml VECTOR_SLICE.sml VectorSlice.sml ARRAY.sig \
  Array.sml ARRAY_SLICE.sml ArraySlice.sml ARRAY2.sig Array2.sml MONO_VECTOR.sml \
  MONO_ARRAY.sml ByteTable.sml MONO_VECTOR_SLICE.sml MONO_ARRAY_SLICE.sml ByteSlice.sml \
  MONO_ARRAY2.sml STRING_CVT.sml StringCvt.sml Char.sml String.sml CHAR.sig \
  STRING.sig SUBSTRING.sig Substring.sml TEXT.sig Text.sml BOOL.sig Bool.sml \
  WORD.sig Word.sml Word64.sml Word63.sml Word32.sml Word31.sml Word8.sml Word16.sml \
  PACK_WORD.sml Pack32Little.sml Pack32Big.sml BYTE.sig Byte.sml INTEGER.sml Int.sml \
  Int32.sml Int31.sml Int63.sml Int64.sml MATH.sig Math.sml REAL.sig Real.sml \
  PACK_REAL.sml PackRealLittle.sml PackRealBig.sml RealArrayVector.sml INT_INF.sml \
  IntInf.sml IO.sml TIME.sig Time.sml RANDOM.sig Random.sml OS_PATH.sml Path.sml \
  OS_FILE_SYS.sml FileSys.sml OS_PROCESS.sml Process.sml OS_IO.sml OS.sml \
  COMMAND_LINE.sml CommandLine.sml DATE.sig Date.sml TIMER.sig Timer.sml \
  NET_HOST_DB.sig NetHostDB.sml SOCKET.sig Socket.sml INET_SOCK.sig BIT_FLAGS.sml \
  POSIX_IO.sml POSIX_PROCESS.sml POSIX_PROCENV.sml POSIX_FILE_SYS.sml POSIX_SIGNAL.sml \
  POSIX_ERROR.sml POSIX_SYS_DB.sml POSIX_TTY.sml POSIX.sig Posix.sml io/stream-io.sig \
  io/stream-io.sml io/imperative-io.sig io/imperative-io.sml io/text-stream-io.sig \
  io/text-io.sig io/text-io.sml io/bin-stream-io.sig io/bin-io.sig io/bin-io.sml \
  UNIX.sig Unix.sml

BASISFILES:=$(BASISFILES0:%=$(BASIS)/%)

.PHONY: test
test:
	$(SIGDOC) -libpath ../jslib ../lib/github.com/diku-dk/*/*.{sig,sml} $(BASISFILES)

.PHONY: clean
clean:
	rm -rf *~ *.html generated_tags.js
