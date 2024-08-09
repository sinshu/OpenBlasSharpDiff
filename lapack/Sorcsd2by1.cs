using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// No description available.
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
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The number of rows in X.
        /// </param>
        /// <param name="p">
        /// [in] P is INTEGER.
        /// The number of rows in X11. 0 &lt;= P &lt;= M.
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
        /// <param name="x21">
        /// [in,out] X21 is REAL array, dimension (LDX21,Q).
        /// On entry, part of the orthogonal matrix whose CSD is desired.
        /// </param>
        /// <param name="ldx21">
        /// [in] LDX21 is INTEGER.
        /// The leading dimension of X21. LDX21 &gt;= MAX(1,M-P).
        /// </param>
        /// <param name="theta">
        /// [out] THETA is REAL array, dimension (R), in which R =.
        /// MIN(P,M-P,Q,M-Q).
        /// C = DIAG( COS(THETA(1)), ... , COS(THETA(R)) ) and
        /// S = DIAG( SIN(THETA(1)), ... , SIN(THETA(R)) ).
        /// </param>
        /// <param name="u1">
        /// [out] U1 is REAL array, dimension (P).
        /// If JOBU1 = &#39;Y&#39;, U1 contains the P-by-P orthogonal matrix U1.
        /// </param>
        /// <param name="ldu1">
        /// [in] LDU1 is INTEGER.
        /// The leading dimension of U1. If JOBU1 = &#39;Y&#39;, LDU1 &gt;=
        /// MAX(1,P).
        /// </param>
        /// <param name="u2">
        /// [out] U2 is REAL array, dimension (M-P).
        /// If JOBU2 = &#39;Y&#39;, U2 contains the (M-P)-by-(M-P) orthogonal
        /// matrix U2.
        /// </param>
        /// <param name="ldu2">
        /// [in] LDU2 is INTEGER.
        /// The leading dimension of U2. If JOBU2 = &#39;Y&#39;, LDU2 &gt;=
        /// MAX(1,M-P).
        /// </param>
        /// <param name="v1t">
        /// [out] V1T is REAL array, dimension (Q).
        /// If JOBV1T = &#39;Y&#39;, V1T contains the Q-by-Q matrix orthogonal
        /// matrix V1**T.
        /// </param>
        /// <param name="ldv1t">
        /// [in] LDV1T is INTEGER.
        /// The leading dimension of V1T. If JOBV1T = &#39;Y&#39;, LDV1T &gt;=
        /// MAX(1,Q).
        /// </param>
        /// <returns>
        /// = 0:  successful exit.
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value.
        /// &gt; 0:  SBBCSD did not converge. See the description of WORK
        /// above for details.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_sorcsd2by1", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Sorcsd2by1(
            MatrixLayout matrixLayout,
            char jobu1,
            char jobu2,
            char jobv1t,
            int m,
            int p,
            int q,
            float* x11,
            int ldx11,
            float* x21,
            int ldx21,
            float* theta,
            float* u1,
            int ldu1,
            float* u2,
            int ldu2,
            float* v1t,
            int ldv1t);
    }
}
