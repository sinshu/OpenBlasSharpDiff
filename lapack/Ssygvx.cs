using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// SSYGVX computes selected eigenvalues, and optionally, eigenvectors
        /// of a real generalized symmetric-definite eigenproblem, of the form
        /// A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A
        /// and B are assumed to be symmetric and B is also positive definite.
        /// Eigenvalues and eigenvectors can be selected by specifying either a
        /// range of values or a range of indices for the desired eigenvalues.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="itype">
        /// [in] ITYPE is INTEGER.
        /// Specifies the problem type to be solved:
        /// = 1:  A*x = (lambda)*B*x
        /// = 2:  A*B*x = (lambda)*x
        /// = 3:  B*A*x = (lambda)*x
        /// </param>
        /// <param name="jobz">
        /// [in] JOBZ is CHARACTER*1.
        /// = &#39;N&#39;:  Compute eigenvalues only;
        /// = &#39;V&#39;:  Compute eigenvalues and eigenvectors.
        /// </param>
        /// <param name="range">
        /// [in] RANGE is CHARACTER*1.
        /// = &#39;A&#39;: all eigenvalues will be found.
        /// = &#39;V&#39;: all eigenvalues in the half-open interval (VL,VU]
        /// will be found.
        /// = &#39;I&#39;: the IL-th through IU-th eigenvalues will be found.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// = &#39;U&#39;:  Upper triangle of A and B are stored;
        /// = &#39;L&#39;:  Lower triangle of A and B are stored.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix pencil (A,B).  N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is REAL array, dimension (LDA, N).
        /// On entry, the symmetric matrix A.  If UPLO = &#39;U&#39;, the
        /// leading N-by-N upper triangular part of A contains the
        /// upper triangular part of the matrix A.  If UPLO = &#39;L&#39;,
        /// the leading N-by-N lower triangular part of A contains
        /// the lower triangular part of the matrix A.
        /// 
        /// On exit, the lower triangle (if UPLO=&#39;L&#39;) or the upper
        /// triangle (if UPLO=&#39;U&#39;) of A, including the diagonal, is
        /// destroyed.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="b">
        /// [in,out] B is REAL array, dimension (LDB, N).
        /// On entry, the symmetric matrix B.  If UPLO = &#39;U&#39;, the
        /// leading N-by-N upper triangular part of B contains the
        /// upper triangular part of the matrix B.  If UPLO = &#39;L&#39;,
        /// the leading N-by-N lower triangular part of B contains
        /// the lower triangular part of the matrix B.
        /// 
        /// On exit, if INFO &lt;= N, the part of B containing the matrix is
        /// overwritten by the triangular factor U or L from the Cholesky
        /// factorization B = U**T*U or B = L*L**T.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B.  LDB &gt;= max(1,N).
        /// </param>
        /// <param name="vl">
        /// [in] VL is REAL.
        /// If RANGE=&#39;V&#39;, the lower bound of the interval to
        /// be searched for eigenvalues. VL &lt; VU.
        /// Not referenced if RANGE = &#39;A&#39; or &#39;I&#39;.
        /// </param>
        /// <param name="vu">
        /// [in] VU is REAL.
        /// If RANGE=&#39;V&#39;, the upper bound of the interval to
        /// be searched for eigenvalues. VL &lt; VU.
        /// Not referenced if RANGE = &#39;A&#39; or &#39;I&#39;.
        /// </param>
        /// <param name="il">
        /// [in] IL is INTEGER.
        /// If RANGE=&#39;I&#39;, the index of the
        /// smallest eigenvalue to be returned.
        /// 1 &lt;= IL &lt;= IU &lt;= N, if N &gt; 0; IL = 1 and IU = 0 if N = 0.
        /// Not referenced if RANGE = &#39;A&#39; or &#39;V&#39;.
        /// </param>
        /// <param name="iu">
        /// [in] IU is INTEGER.
        /// If RANGE=&#39;I&#39;, the index of the
        /// largest eigenvalue to be returned.
        /// 1 &lt;= IL &lt;= IU &lt;= N, if N &gt; 0; IL = 1 and IU = 0 if N = 0.
        /// Not referenced if RANGE = &#39;A&#39; or &#39;V&#39;.
        /// </param>
        /// <param name="abstol">
        /// [in] ABSTOL is REAL.
        /// The absolute error tolerance for the eigenvalues.
        /// An approximate eigenvalue is accepted as converged
        /// when it is determined to lie in an interval [a,b]
        /// of width less than or equal to
        /// 
        /// ABSTOL + EPS *   max( |a|,|b| ) ,
        /// 
        /// where EPS is the machine precision.  If ABSTOL is less than
        /// or equal to zero, then  EPS*|T|  will be used in its place,
        /// where |T| is the 1-norm of the tridiagonal matrix obtained
        /// by reducing C to tridiagonal form, where C is the symmetric
        /// matrix of the standard symmetric problem to which the
        /// generalized problem is transformed.
        /// 
        /// Eigenvalues will be computed most accurately when ABSTOL is
        /// set to twice the underflow threshold 2*DLAMCH(&#39;S&#39;), not zero.
        /// If this routine returns with INFO&gt;0, indicating that some
        /// eigenvectors did not converge, try setting ABSTOL to
        /// 2*SLAMCH(&#39;S&#39;).
        /// </param>
        /// <param name="m">
        /// [out] M is INTEGER.
        /// The total number of eigenvalues found.  0 &lt;= M &lt;= N.
        /// If RANGE = &#39;A&#39;, M = N, and if RANGE = &#39;I&#39;, M = IU-IL+1.
        /// </param>
        /// <param name="w">
        /// [out] W is REAL array, dimension (N).
        /// On normal exit, the first M elements contain the selected
        /// eigenvalues in ascending order.
        /// </param>
        /// <param name="z">
        /// [out] Z is REAL array, dimension (LDZ, max(1,M)).
        /// If JOBZ = &#39;N&#39;, then Z is not referenced.
        /// If JOBZ = &#39;V&#39;, then if INFO = 0, the first M columns of Z
        /// contain the orthonormal eigenvectors of the matrix A
        /// corresponding to the selected eigenvalues, with the i-th
        /// column of Z holding the eigenvector associated with W(i).
        /// The eigenvectors are normalized as follows:
        /// if ITYPE = 1 or 2, Z**T*B*Z = I;
        /// if ITYPE = 3, Z**T*inv(B)*Z = I.
        /// 
        /// If an eigenvector fails to converge, then that column of Z
        /// contains the latest approximation to the eigenvector, and the
        /// index of the eigenvector is returned in IFAIL.
        /// Note: the user must ensure that at least max(1,M) columns are
        /// supplied in the array Z; if RANGE = &#39;V&#39;, the exact value of M
        /// is not known in advance and an upper bound must be used.
        /// </param>
        /// <param name="ldz">
        /// [in] LDZ is INTEGER.
        /// The leading dimension of the array Z.  LDZ &gt;= 1, and if
        /// JOBZ = &#39;V&#39;, LDZ &gt;= max(1,N).
        /// </param>
        /// <param name="ifail">
        /// [out] IFAIL is INTEGER array, dimension (N).
        /// If JOBZ = &#39;V&#39;, then if INFO = 0, the first M elements of
        /// IFAIL are zero.  If INFO &gt; 0, then IFAIL contains the
        /// indices of the eigenvectors that failed to converge.
        /// If JOBZ = &#39;N&#39;, then IFAIL is not referenced.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0:  SPOTRF or SSYEVX returned an error code:
        /// &lt;= N:  if INFO = i, SSYEVX failed to converge;
        /// i eigenvectors failed to converge.  Their indices
        /// are stored in array IFAIL.
        /// &gt; N:   if INFO = N + i, for 1 &lt;= i &lt;= N, then the leading
        /// principal minor of order i of B is not positive.
        /// The factorization of B could not be completed and
        /// no eigenvalues or eigenvectors were computed.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_ssygvx", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Ssygvx(
            MatrixLayout matrixLayout,
            int itype,
            char jobz,
            char range,
            char uplo,
            int n,
            float* a,
            int lda,
            float* b,
            int ldb,
            float vl,
            float vu,
            int il,
            int iu,
            float abstol,
            int* m,
            float* w,
            float* z,
            int ldz,
            int* ifail);
    }
}
