using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DTGEVC computes some or all of the right and/or left eigenvectors of
        /// a pair of real matrices (S,P), where S is a quasi-triangular matrix
        /// and P is upper triangular.  Matrix pairs of this type are produced by
        /// the generalized Schur factorization of a matrix pair (A,B):
        /// </para>
        /// <para>
        ///    A = Q*S*Z**T,  B = Q*P*Z**T
        /// </para>
        /// <para>
        /// as computed by DGGHRD + DHGEQZ.
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
        /// directly from the diagonal blocks of S and P.
        /// </para>
        /// <para>
        /// This routine returns the matrices X and/or Y of right and left
        /// eigenvectors of (S,P), or the products Z*X and/or Q*Y,
        /// where Z and Q are input matrices.
        /// If Q and Z are the orthogonal factors from the generalized Schur
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
        /// computed.  If w(j) is a real eigenvalue, the corresponding
        /// real eigenvector is computed if SELECT(j) is .TRUE..
        /// If w(j) and w(j+1) are the real and imaginary parts of a
        /// complex eigenvalue, the corresponding complex eigenvector
        /// is computed if either SELECT(j) or SELECT(j+1) is .TRUE.,
        /// and on exit SELECT(j) is set to .TRUE. and SELECT(j+1) is
        /// set to .FALSE..
        /// Not referenced if HOWMNY = &#39;A&#39; or &#39;B&#39;.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrices S and P.  N &gt;= 0.
        /// </param>
        /// <param name="s">
        /// [in] S is DOUBLE PRECISION array, dimension (LDS,N).
        /// The upper quasi-triangular matrix S from a generalized Schur
        /// factorization, as computed by DHGEQZ.
        /// </param>
        /// <param name="lds">
        /// [in] LDS is INTEGER.
        /// The leading dimension of array S.  LDS &gt;= max(1,N).
        /// </param>
        /// <param name="p">
        /// [in] P is DOUBLE PRECISION array, dimension (LDP,N).
        /// The upper triangular matrix P from a generalized Schur
        /// factorization, as computed by DHGEQZ.
        /// 2-by-2 diagonal blocks of P corresponding to 2-by-2 blocks
        /// of S must be in positive diagonal form.
        /// </param>
        /// <param name="ldp">
        /// [in] LDP is INTEGER.
        /// The leading dimension of array P.  LDP &gt;= max(1,N).
        /// </param>
        /// <param name="vl">
        /// [in,out] VL is DOUBLE PRECISION array, dimension (LDVL,MM).
        /// On entry, if SIDE = &#39;L&#39; or &#39;B&#39; and HOWMNY = &#39;B&#39;, VL must
        /// contain an N-by-N matrix Q (usually the orthogonal matrix Q
        /// of left Schur vectors returned by DHGEQZ).
        /// On exit, if SIDE = &#39;L&#39; or &#39;B&#39;, VL contains:
        /// if HOWMNY = &#39;A&#39;, the matrix Y of left eigenvectors of (S,P);
        /// if HOWMNY = &#39;B&#39;, the matrix Q*Y;
        /// if HOWMNY = &#39;S&#39;, the left eigenvectors of (S,P) specified by
        /// SELECT, stored consecutively in the columns of
        /// VL, in the same order as their eigenvalues.
        /// 
        /// A complex eigenvector corresponding to a complex eigenvalue
        /// is stored in two consecutive columns, the first holding the
        /// real part, and the second the imaginary part.
        /// 
        /// Not referenced if SIDE = &#39;R&#39;.
        /// </param>
        /// <param name="ldvl">
        /// [in] LDVL is INTEGER.
        /// The leading dimension of array VL.  LDVL &gt;= 1, and if
        /// SIDE = &#39;L&#39; or &#39;B&#39;, LDVL &gt;= N.
        /// </param>
        /// <param name="vr">
        /// [in,out] VR is DOUBLE PRECISION array, dimension (LDVR,MM).
        /// On entry, if SIDE = &#39;R&#39; or &#39;B&#39; and HOWMNY = &#39;B&#39;, VR must
        /// contain an N-by-N matrix Z (usually the orthogonal matrix Z
        /// of right Schur vectors returned by DHGEQZ).
        /// 
        /// On exit, if SIDE = &#39;R&#39; or &#39;B&#39;, VR contains:
        /// if HOWMNY = &#39;A&#39;, the matrix X of right eigenvectors of (S,P);
        /// if HOWMNY = &#39;B&#39; or &#39;b&#39;, the matrix Z*X;
        /// if HOWMNY = &#39;S&#39; or &#39;s&#39;, the right eigenvectors of (S,P)
        /// specified by SELECT, stored consecutively in the
        /// columns of VR, in the same order as their
        /// eigenvalues.
        /// 
        /// A complex eigenvector corresponding to a complex eigenvalue
        /// is stored in two consecutive columns, the first holding the
        /// real part and the second the imaginary part.
        /// 
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
        /// is set to N.  Each selected real eigenvector occupies one
        /// column and each selected complex eigenvector occupies two
        /// columns.
        /// </param>
        /// <returns>
        /// = 0:  successful exit.
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value.
        /// &gt; 0:  the 2-by-2 block (INFO:INFO+1) does not have a complex
        /// eigenvalue.
        /// </returns>
        /// <remarks>
        /// <para>
        ///  Allocation of workspace:
        ///  ---------- -- ---------
        /// </para>
        /// <para>
        ///     WORK( j ) = 1-norm of j-th column of A, above the diagonal
        ///     WORK( N+j ) = 1-norm of j-th column of B, above the diagonal
        ///     WORK( 2*N+1:3*N ) = real part of eigenvector
        ///     WORK( 3*N+1:4*N ) = imaginary part of eigenvector
        ///     WORK( 4*N+1:5*N ) = real part of back-transformed eigenvector
        ///     WORK( 5*N+1:6*N ) = imaginary part of back-transformed eigenvector
        /// </para>
        /// <para>
        ///  Rowwise vs. columnwise solution methods:
        ///  ------- --  ---------- -------- -------
        /// </para>
        /// <para>
        ///  Finding a generalized eigenvector consists basically of solving the
        ///  singular triangular system
        /// </para>
        /// <para>
        ///   (A - w B) x = 0     (for right) or:   (A - w B)**H y = 0  (for left)
        /// </para>
        /// <para>
        ///  Consider finding the i-th right eigenvector (assume all eigenvalues
        ///  are real). The equation to be solved is:
        ///       n                   i
        ///  0 = sum  C(j,k) v(k)  = sum  C(j,k) v(k)     for j = i,. . .,1
        ///      k=j                 k=j
        /// </para>
        /// <para>
        ///  where  C = (A - w B)  (The components v(i+1:n) are 0.)
        /// </para>
        /// <para>
        ///  The &quot;rowwise&quot; method is:
        /// </para>
        /// <para>
        ///  (1)  v(i) := 1
        ///  for j = i-1,. . .,1:
        ///                          i
        ///      (2) compute  s = - sum C(j,k) v(k)   and
        ///                        k=j+1
        /// </para>
        /// <para>
        ///      (3) v(j) := s / C(j,j)
        /// </para>
        /// <para>
        ///  Step 2 is sometimes called the &quot;dot product&quot; step, since it is an
        ///  inner product between the j-th row and the portion of the eigenvector
        ///  that has been computed so far.
        /// </para>
        /// <para>
        ///  The &quot;columnwise&quot; method consists basically in doing the sums
        ///  for all the rows in parallel.  As each v(j) is computed, the
        ///  contribution of v(j) times the j-th column of C is added to the
        ///  partial sums.  Since FORTRAN arrays are stored columnwise, this has
        ///  the advantage that at each step, the elements of C that are accessed
        ///  are adjacent to one another, whereas with the rowwise method, the
        ///  elements accessed at a step are spaced LDS (and LDP) words apart.
        /// </para>
        /// <para>
        ///  When finding left eigenvectors, the matrix in question is the
        ///  transpose of the one in storage, so the rowwise method then
        ///  actually accesses columns of A and B at each step, and so is the
        ///  preferred method.
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dtgevc", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dtgevc(
            MatrixLayout matrixLayout,
            char side,
            char howmny,
            bool* select,
            int n,
            double* s,
            int lds,
            double* p,
            int ldp,
            double* vl,
            int ldvl,
            double* vr,
            int ldvr,
            int mm,
            int* m);
    }
}
