using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CTREVC computes some or all of the right and/or left eigenvectors of
        /// a complex upper triangular matrix T.
        /// Matrices of this type are produced by the Schur factorization of
        /// a complex general matrix:  A = Q*T*Q**H, as computed by CHSEQR.
        /// </para>
        /// <para>
        /// The right eigenvector x and the left eigenvector y of T corresponding
        /// to an eigenvalue w are defined by:
        /// </para>
        /// <para>
        ///              T*x = w*x,     (y**H)*T = w*(y**H)
        /// </para>
        /// <para>
        /// where y**H denotes the conjugate transpose of the vector y.
        /// The eigenvalues are not input to this routine, but are read directly
        /// from the diagonal of T.
        /// </para>
        /// <para>
        /// This routine returns the matrices X and/or Y of right and left
        /// eigenvectors of T, or the products Q*X and/or Q*Y, where Q is an
        /// input matrix.  If Q is the unitary factor that reduces a matrix A to
        /// Schur form T, then Q*X and Q*Y are the matrices of right and left
        /// eigenvectors of A.
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
        /// backtransformed using the matrices supplied in
        /// VR and/or VL;
        /// = &#39;S&#39;:  compute selected right and/or left eigenvectors,
        /// as indicated by the logical array SELECT.
        /// </param>
        /// <param name="select">
        /// [in] SELECT is LOGICAL array, dimension (N).
        /// If HOWMNY = &#39;S&#39;, SELECT specifies the eigenvectors to be
        /// computed.
        /// The eigenvector corresponding to the j-th eigenvalue is
        /// computed if SELECT(j) = .TRUE..
        /// Not referenced if HOWMNY = &#39;A&#39; or &#39;B&#39;.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix T. N &gt;= 0.
        /// </param>
        /// <param name="t">
        /// [in,out] T is COMPLEX array, dimension (LDT,N).
        /// The upper triangular matrix T.  T is modified, but restored
        /// on exit.
        /// </param>
        /// <param name="ldt">
        /// [in] LDT is INTEGER.
        /// The leading dimension of the array T. LDT &gt;= max(1,N).
        /// </param>
        /// <param name="vl">
        /// [in,out] VL is COMPLEX array, dimension (LDVL,MM).
        /// On entry, if SIDE = &#39;L&#39; or &#39;B&#39; and HOWMNY = &#39;B&#39;, VL must
        /// contain an N-by-N matrix Q (usually the unitary matrix Q of
        /// Schur vectors returned by CHSEQR).
        /// On exit, if SIDE = &#39;L&#39; or &#39;B&#39;, VL contains:
        /// if HOWMNY = &#39;A&#39;, the matrix Y of left eigenvectors of T;
        /// if HOWMNY = &#39;B&#39;, the matrix Q*Y;
        /// if HOWMNY = &#39;S&#39;, the left eigenvectors of T specified by
        /// SELECT, stored consecutively in the columns
        /// of VL, in the same order as their
        /// eigenvalues.
        /// Not referenced if SIDE = &#39;R&#39;.
        /// </param>
        /// <param name="ldvl">
        /// [in] LDVL is INTEGER.
        /// The leading dimension of the array VL.  LDVL &gt;= 1, and if
        /// SIDE = &#39;L&#39; or &#39;B&#39;, LDVL &gt;= N.
        /// </param>
        /// <param name="vr">
        /// [in,out] VR is COMPLEX array, dimension (LDVR,MM).
        /// On entry, if SIDE = &#39;R&#39; or &#39;B&#39; and HOWMNY = &#39;B&#39;, VR must
        /// contain an N-by-N matrix Q (usually the unitary matrix Q of
        /// Schur vectors returned by CHSEQR).
        /// On exit, if SIDE = &#39;R&#39; or &#39;B&#39;, VR contains:
        /// if HOWMNY = &#39;A&#39;, the matrix X of right eigenvectors of T;
        /// if HOWMNY = &#39;B&#39;, the matrix Q*X;
        /// if HOWMNY = &#39;S&#39;, the right eigenvectors of T specified by
        /// SELECT, stored consecutively in the columns
        /// of VR, in the same order as their
        /// eigenvalues.
        /// Not referenced if SIDE = &#39;L&#39;.
        /// </param>
        /// <param name="ldvr">
        /// [in] LDVR is INTEGER.
        /// The leading dimension of the array VR.  LDVR &gt;= 1, and if
        /// SIDE = &#39;R&#39; or &#39;B&#39;; LDVR &gt;= N.
        /// </param>
        /// <param name="mm">
        /// [in] MM is INTEGER.
        /// The number of columns in the arrays VL and/or VR. MM &gt;= M.
        /// </param>
        /// <param name="m">
        /// [out] M is INTEGER.
        /// The number of columns in the arrays VL and/or VR actually
        /// used to store the eigenvectors.  If HOWMNY = &#39;A&#39; or &#39;B&#39;, M
        /// is set to N.  Each selected eigenvector occupies one
        /// column.
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
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_ctrevc", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Ctrevc(
            MatrixLayout matrixLayout,
            char side,
            char howmny,
            bool* select,
            int n,
            Complex32* t,
            int ldt,
            Complex32* vl,
            int ldvl,
            Complex32* vr,
            int ldvr,
            int mm,
            int* m);
    }
}
