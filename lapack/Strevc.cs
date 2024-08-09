using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// STREVC computes some or all of the right and/or left eigenvectors of
        /// a real upper quasi-triangular matrix T.
        /// Matrices of this type are produced by the Schur factorization of
        /// a real general matrix:  A = Q*T*Q**T, as computed by SHSEQR.
        /// </para>
        /// <para>
        /// The right eigenvector x and the left eigenvector y of T corresponding
        /// to an eigenvalue w are defined by:
        /// </para>
        /// <para>
        ///    T*x = w*x,     (y**H)*T = w*(y**H)
        /// </para>
        /// <para>
        /// where y**H denotes the conjugate transpose of y.
        /// The eigenvalues are not input to this routine, but are read directly
        /// from the diagonal blocks of T.
        /// </para>
        /// <para>
        /// This routine returns the matrices X and/or Y of right and left
        /// eigenvectors of T, or the products Q*X and/or Q*Y, where Q is an
        /// input matrix.  If Q is the orthogonal factor that reduces a matrix
        /// A to Schur form T, then Q*X and Q*Y are the matrices of right and
        /// left eigenvectors of A.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="side">
        /// [in] SIDE is CHARACTER*1.
        /// = &#39;R&#39;:  compute right eigenvectors only;
        /// = &#39;L&#39;:  compute left eigenvectors only;
        /// = &#39;B&#39;:  compute both right and left eigenvectors.
        /// </param>
        /// <param name="howmny">
        /// [in] HOWMNY is CHARACTER*1.
        /// = &#39;A&#39;:  compute all right and/or left eigenvectors;
        /// = &#39;B&#39;:  compute all right and/or left eigenvectors,
        /// backtransformed by the matrices in VR and/or VL;
        /// = &#39;S&#39;:  compute selected right and/or left eigenvectors,
        /// as indicated by the logical array SELECT.
        /// </param>
        /// <param name="select">
        /// [in,out] SELECT is LOGICAL array, dimension (N).
        /// If HOWMNY = &#39;S&#39;, SELECT specifies the eigenvectors to be
        /// computed.
        /// If w(j) is a real eigenvalue, the corresponding real
        /// eigenvector is computed if SELECT(j) is .TRUE..
        /// If w(j) and w(j+1) are the real and imaginary parts of a
        /// complex eigenvalue, the corresponding complex eigenvector is
        /// computed if either SELECT(j) or SELECT(j+1) is .TRUE., and
        /// on exit SELECT(j) is set to .TRUE. and SELECT(j+1) is set to
        /// .FALSE..
        /// Not referenced if HOWMNY = &#39;A&#39; or &#39;B&#39;.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix T. N &gt;= 0.
        /// </param>
        /// <param name="t">
        /// [in] T is REAL array, dimension (LDT,N).
        /// The upper quasi-triangular matrix T in Schur canonical form.
        /// </param>
        /// <param name="ldt">
        /// [in] LDT is INTEGER.
        /// The leading dimension of the array T. LDT &gt;= max(1,N).
        /// </param>
        /// <param name="vl">
        /// [in,out] VL is REAL array, dimension (LDVL,MM).
        /// On entry, if SIDE = &#39;L&#39; or &#39;B&#39; and HOWMNY = &#39;B&#39;, VL must
        /// contain an N-by-N matrix Q (usually the orthogonal matrix Q
        /// of Schur vectors returned by SHSEQR).
        /// On exit, if SIDE = &#39;L&#39; or &#39;B&#39;, VL contains:
        /// if HOWMNY = &#39;A&#39;, the matrix Y of left eigenvectors of T;
        /// if HOWMNY = &#39;B&#39;, the matrix Q*Y;
        /// if HOWMNY = &#39;S&#39;, the left eigenvectors of T specified by
        /// SELECT, stored consecutively in the columns
        /// of VL, in the same order as their
        /// eigenvalues.
        /// A complex eigenvector corresponding to a complex eigenvalue
        /// is stored in two consecutive columns, the first holding the
        /// real part, and the second the imaginary part.
        /// Not referenced if SIDE = &#39;R&#39;.
        /// </param>
        /// <param name="ldvl">
        /// [in] LDVL is INTEGER.
        /// The leading dimension of the array VL.  LDVL &gt;= 1, and if
        /// SIDE = &#39;L&#39; or &#39;B&#39;, LDVL &gt;= N.
        /// </param>
        /// <param name="vr">
        /// [in,out] VR is REAL array, dimension (LDVR,MM).
        /// On entry, if SIDE = &#39;R&#39; or &#39;B&#39; and HOWMNY = &#39;B&#39;, VR must
        /// contain an N-by-N matrix Q (usually the orthogonal matrix Q
        /// of Schur vectors returned by SHSEQR).
        /// On exit, if SIDE = &#39;R&#39; or &#39;B&#39;, VR contains:
        /// if HOWMNY = &#39;A&#39;, the matrix X of right eigenvectors of T;
        /// if HOWMNY = &#39;B&#39;, the matrix Q*X;
        /// if HOWMNY = &#39;S&#39;, the right eigenvectors of T specified by
        /// SELECT, stored consecutively in the columns
        /// of VR, in the same order as their
        /// eigenvalues.
        /// A complex eigenvector corresponding to a complex eigenvalue
        /// is stored in two consecutive columns, the first holding the
        /// real part and the second the imaginary part.
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
        /// used to store the eigenvectors.
        /// If HOWMNY = &#39;A&#39; or &#39;B&#39;, M is set to N.
        /// Each selected real eigenvector occupies one column and each
        /// selected complex eigenvector occupies two columns.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        /// <remarks>
        /// <para>
        ///  The algorithm used in this program is basically backward (forward)
        ///  substitution, with scaling to make the the code robust against
        ///  possible overflow.
        /// </para>
        /// <para>
        ///  Each eigenvector is normalized so that the element of largest
        ///  magnitude has magnitude 1; here the magnitude of a complex number
        ///  (x,y) is taken to be |x| + |y|.
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_strevc", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Strevc(
            MatrixLayout matrixLayout,
            char side,
            char howmny,
            bool* select,
            int n,
            float* t,
            int ldt,
            float* vl,
            int ldvl,
            float* vr,
            int ldvr,
            int mm,
            int* m);
    }
}
