using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZTGEVC computes some or all of the right and/or left eigenvectors of
        /// a pair of complex matrices (S,P), where S and P are upper triangular.
        /// Matrix pairs of this type are produced by the generalized Schur
        /// factorization of a complex matrix pair (A,B):
        /// </para>
        /// <para>
        ///    A = Q*S*Z**H,  B = Q*P*Z**H
        /// </para>
        /// <para>
        /// as computed by ZGGHRD + ZHGEQZ.
        /// </para>
        /// <para>
        /// The right eigenvector x and the left eigenvector y of (S,P)
        /// corresponding to an eigenvalue w are defined by:
        /// </para>
        /// <para>
        ///    S*x = w*P*x,  (y**H)*S = w*(y**H)*P,
        /// </para>
        /// <para>
        /// where y**H denotes the conjugate transpose of y.
        /// The eigenvalues are not input to this routine, but are computed
        /// directly from the diagonal elements of S and P.
        /// </para>
        /// <para>
        /// This routine returns the matrices X and/or Y of right and left
        /// eigenvectors of (S,P), or the products Z*X and/or Q*Y,
        /// where Z and Q are input matrices.
        /// If Q and Z are the unitary factors from the generalized Schur
        /// factorization of a matrix pair (A,B), then Z*X and Q*Y
        /// are the matrices of right and left eigenvectors of (A,B).
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="side">
        /// [in] SIDE is CHARACTER*1.
        /// = &#39;R&#39;: compute right eigenvectors only;
        /// = &#39;L&#39;: compute left eigenvectors only;
        /// = &#39;B&#39;: compute both right and left eigenvectors.
        /// </param>
        /// <param name="howmny">
        /// [in] HOWMNY is CHARACTER*1.
        /// = &#39;A&#39;: compute all right and/or left eigenvectors;
        /// = &#39;B&#39;: compute all right and/or left eigenvectors,
        /// backtransformed by the matrices in VR and/or VL;
        /// = &#39;S&#39;: compute selected right and/or left eigenvectors,
        /// specified by the logical array SELECT.
        /// </param>
        /// <param name="select">
        /// [in] SELECT is LOGICAL array, dimension (N).
        /// If HOWMNY=&#39;S&#39;, SELECT specifies the eigenvectors to be
        /// computed.  The eigenvector corresponding to the j-th
        /// eigenvalue is computed if SELECT(j) = .TRUE..
        /// Not referenced if HOWMNY = &#39;A&#39; or &#39;B&#39;.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrices S and P.  N &gt;= 0.
        /// </param>
        /// <param name="s">
        /// [in] S is COMPLEX*16 array, dimension (LDS,N).
        /// The upper triangular matrix S from a generalized Schur
        /// factorization, as computed by ZHGEQZ.
        /// </param>
        /// <param name="lds">
        /// [in] LDS is INTEGER.
        /// The leading dimension of array S.  LDS &gt;= max(1,N).
        /// </param>
        /// <param name="p">
        /// [in] P is COMPLEX*16 array, dimension (LDP,N).
        /// The upper triangular matrix P from a generalized Schur
        /// factorization, as computed by ZHGEQZ.  P must have real
        /// diagonal elements.
        /// </param>
        /// <param name="ldp">
        /// [in] LDP is INTEGER.
        /// The leading dimension of array P.  LDP &gt;= max(1,N).
        /// </param>
        /// <param name="vl">
        /// [in,out] VL is COMPLEX*16 array, dimension (LDVL,MM).
        /// On entry, if SIDE = &#39;L&#39; or &#39;B&#39; and HOWMNY = &#39;B&#39;, VL must
        /// contain an N-by-N matrix Q (usually the unitary matrix Q
        /// of left Schur vectors returned by ZHGEQZ).
        /// On exit, if SIDE = &#39;L&#39; or &#39;B&#39;, VL contains:
        /// if HOWMNY = &#39;A&#39;, the matrix Y of left eigenvectors of (S,P);
        /// if HOWMNY = &#39;B&#39;, the matrix Q*Y;
        /// if HOWMNY = &#39;S&#39;, the left eigenvectors of (S,P) specified by
        /// SELECT, stored consecutively in the columns of
        /// VL, in the same order as their eigenvalues.
        /// Not referenced if SIDE = &#39;R&#39;.
        /// </param>
        /// <param name="ldvl">
        /// [in] LDVL is INTEGER.
        /// The leading dimension of array VL.  LDVL &gt;= 1, and if
        /// SIDE = &#39;L&#39; or &#39;l&#39; or &#39;B&#39; or &#39;b&#39;, LDVL &gt;= N.
        /// </param>
        /// <param name="vr">
        /// [in,out] VR is COMPLEX*16 array, dimension (LDVR,MM).
        /// On entry, if SIDE = &#39;R&#39; or &#39;B&#39; and HOWMNY = &#39;B&#39;, VR must
        /// contain an N-by-N matrix Z (usually the unitary matrix Z
        /// of right Schur vectors returned by ZHGEQZ).
        /// On exit, if SIDE = &#39;R&#39; or &#39;B&#39;, VR contains:
        /// if HOWMNY = &#39;A&#39;, the matrix X of right eigenvectors of (S,P);
        /// if HOWMNY = &#39;B&#39;, the matrix Z*X;
        /// if HOWMNY = &#39;S&#39;, the right eigenvectors of (S,P) specified by
        /// SELECT, stored consecutively in the columns of
        /// VR, in the same order as their eigenvalues.
        /// Not referenced if SIDE = &#39;L&#39;.
        /// </param>
        /// <param name="ldvr">
        /// [in] LDVR is INTEGER.
        /// The leading dimension of the array VR.  LDVR &gt;= 1, and if
        /// SIDE = &#39;R&#39; or &#39;B&#39;, LDVR &gt;= N.
        /// </param>
        /// <param name="mm">
        /// [in] MM is INTEGER.
        /// The number of columns in the arrays VL and/or VR. MM &gt;= M.
        /// </param>
        /// <param name="m">
        /// [out] M is INTEGER.
        /// The number of columns in the arrays VL and/or VR actually
        /// used to store the eigenvectors.  If HOWMNY = &#39;A&#39; or &#39;B&#39;, M
        /// is set to N.  Each selected eigenvector occupies one column.
        /// </param>
        /// <returns>
        /// = 0:  successful exit.
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_ztgevc", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Ztgevc(
            MatrixLayout matrixLayout,
            char side,
            char howmny,
            bool* select,
            int n,
            Complex* s,
            int lds,
            Complex* p,
            int ldp,
            Complex* vl,
            int ldvl,
            Complex* vr,
            int ldvr,
            int mm,
            int* m);
    }
}
