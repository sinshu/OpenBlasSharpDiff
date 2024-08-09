using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// SORCSD computes the CS decomposition of an M-by-M partitioned
        /// orthogonal matrix X:
        /// </para>
        /// <para>
        ///                                 [  I  0  0 |  0  0  0 ]
        ///                                 [  0  C  0 |  0 -S  0 ]
        ///     [ X11 | X12 ]   [ U1 |    ] [  0  0  0 |  0  0 -I ] [ V1 |    ]**T
        /// X = [-----------] = [---------] [---------------------] [---------]   .
        ///     [ X21 | X22 ]   [    | U2 ] [  0  0  0 |  I  0  0 ] [    | V2 ]
        ///                                 [  0  S  0 |  0  C  0 ]
        ///                                 [  0  0  I |  0  0  0 ]
        /// </para>
        /// <para>
        /// X11 is P-by-Q. The orthogonal matrices U1, U2, V1, and V2 are P-by-P,
        /// (M-P)-by-(M-P), Q-by-Q, and (M-Q)-by-(M-Q), respectively. C and S are
        /// R-by-R nonnegative diagonal matrices satisfying C^2 + S^2 = I, in
        /// which R = MIN(P,M-P,Q,M-Q).
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="jobu1">
        /// [in] JOBU1 is CHARACTER.
        /// = &#39;Y&#39;:      U1 is computed;
        /// otherwise:  U1 is not computed.
        /// </param>
        /// <param name="jobu2">
        /// [in] JOBU2 is CHARACTER.
        /// = &#39;Y&#39;:      U2 is computed;
        /// otherwise:  U2 is not computed.
        /// </param>
        /// <param name="jobv1t">
        /// [in] JOBV1T is CHARACTER.
        /// = &#39;Y&#39;:      V1T is computed;
        /// otherwise:  V1T is not computed.
        /// </param>
        /// <param name="jobv2t">
        /// [in] JOBV2T is CHARACTER.
        /// = &#39;Y&#39;:      V2T is computed;
        /// otherwise:  V2T is not computed.
        /// </param>
        /// <param name="trans">
        /// [in] TRANS is CHARACTER.
        /// = &#39;T&#39;:      X, U1, U2, V1T, and V2T are stored in row-major
        /// order;
        /// otherwise:  X, U1, U2, V1T, and V2T are stored in column-
        /// major order.
        /// </param>
        /// <param name="signs">
        /// [in] SIGNS is CHARACTER.
        /// = &#39;O&#39;:      The lower-left block is made nonpositive (the
        /// &quot;other&quot; convention);
        /// otherwise:  The upper-right block is made nonpositive (the
        /// &quot;default&quot; convention).
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The number of rows and columns in X.
        /// </param>
        /// <param name="p">
        /// [in] P is INTEGER.
        /// The number of rows in X11 and X12. 0 &lt;= P &lt;= M.
        /// </param>
        /// <param name="q">
        /// [in] Q is INTEGER.
        /// The number of columns in X11 and X21. 0 &lt;= Q &lt;= M.
        /// </param>
        /// <param name="x11">
        /// [in,out] X11 is REAL array, dimension (LDX11,Q).
        /// On entry, part of the orthogonal matrix whose CSD is desired.
        /// </param>
        /// <param name="ldx11">
        /// [in] LDX11 is INTEGER.
        /// The leading dimension of X11. LDX11 &gt;= MAX(1,P).
        /// </param>
        /// <param name="x12">
        /// [in,out] X12 is REAL array, dimension (LDX12,M-Q).
        /// On entry, part of the orthogonal matrix whose CSD is desired.
        /// </param>
        /// <param name="ldx12">
        /// [in] LDX12 is INTEGER.
        /// The leading dimension of X12. LDX12 &gt;= MAX(1,P).
        /// </param>
        /// <param name="x21">
        /// [in,out] X21 is REAL array, dimension (LDX21,Q).
        /// On entry, part of the orthogonal matrix whose CSD is desired.
        /// </param>
        /// <param name="ldx21">
        /// [in] LDX21 is INTEGER.
        /// The leading dimension of X11. LDX21 &gt;= MAX(1,M-P).
        /// </param>
        /// <param name="x22">
        /// [in,out] X22 is REAL array, dimension (LDX22,M-Q).
        /// On entry, part of the orthogonal matrix whose CSD is desired.
        /// </param>
        /// <param name="ldx22">
        /// [in] LDX22 is INTEGER.
        /// The leading dimension of X11. LDX22 &gt;= MAX(1,M-P).
        /// </param>
        /// <param name="theta">
        /// [out] THETA is REAL array, dimension (R), in which R =.
        /// MIN(P,M-P,Q,M-Q).
        /// C = DIAG( COS(THETA(1)), ... , COS(THETA(R)) ) and
        /// S = DIAG( SIN(THETA(1)), ... , SIN(THETA(R)) ).
        /// </param>
        /// <param name="u1">
        /// [out] U1 is REAL array, dimension (LDU1,P).
        /// If JOBU1 = &#39;Y&#39;, U1 contains the P-by-P orthogonal matrix U1.
        /// </param>
        /// <param name="ldu1">
        /// [in] LDU1 is INTEGER.
        /// The leading dimension of U1. If JOBU1 = &#39;Y&#39;, LDU1 &gt;=
        /// MAX(1,P).
        /// </param>
        /// <param name="u2">
        /// [out] U2 is REAL array, dimension (LDU2,M-P).
        /// If JOBU2 = &#39;Y&#39;, U2 contains the (M-P)-by-(M-P) orthogonal
        /// matrix U2.
        /// </param>
        /// <param name="ldu2">
        /// [in] LDU2 is INTEGER.
        /// The leading dimension of U2. If JOBU2 = &#39;Y&#39;, LDU2 &gt;=
        /// MAX(1,M-P).
        /// </param>
        /// <param name="v1t">
        /// [out] V1T is REAL array, dimension (LDV1T,Q).
        /// If JOBV1T = &#39;Y&#39;, V1T contains the Q-by-Q matrix orthogonal
        /// matrix V1**T.
        /// </param>
        /// <param name="ldv1t">
        /// [in] LDV1T is INTEGER.
        /// The leading dimension of V1T. If JOBV1T = &#39;Y&#39;, LDV1T &gt;=
        /// MAX(1,Q).
        /// </param>
        /// <param name="v2t">
        /// [out] V2T is REAL array, dimension (LDV2T,M-Q).
        /// If JOBV2T = &#39;Y&#39;, V2T contains the (M-Q)-by-(M-Q) orthogonal
        /// matrix V2**T.
        /// </param>
        /// <param name="ldv2t">
        /// [in] LDV2T is INTEGER.
        /// The leading dimension of V2T. If JOBV2T = &#39;Y&#39;, LDV2T &gt;=
        /// MAX(1,M-Q).
        /// </param>
        /// <returns>
        /// = 0:  successful exit.
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value.
        /// &gt; 0:  SBBCSD did not converge. See the description of WORK
        /// above for details.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_sorcsd", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Sorcsd(
            MatrixLayout matrixLayout,
            char jobu1,
            char jobu2,
            char jobv1t,
            char jobv2t,
            char trans,
            char signs,
            int m,
            int p,
            int q,
            float* x11,
            int ldx11,
            float* x12,
            int ldx12,
            float* x21,
            int ldx21,
            float* x22,
            int ldx22,
            float* theta,
            float* u1,
            int ldu1,
            float* u2,
            int ldu2,
            float* v1t,
            int ldv1t,
            float* v2t,
            int ldv2t);
    }
}
