using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZHEEVR_2STAGE computes selected eigenvalues and, optionally, eigenvectors
        /// of a complex Hermitian matrix A using the 2stage technique for
        /// the reduction to tridiagonal.  Eigenvalues and eigenvectors can
        /// be selected by specifying either a range of values or a range of
        /// indices for the desired eigenvalues.
        /// </para>
        /// <para>
        /// ZHEEVR_2STAGE first reduces the matrix A to tridiagonal form T with a call
        /// to ZHETRD.  Then, whenever possible, ZHEEVR_2STAGE calls ZSTEMR to compute
        /// eigenspectrum using Relatively Robust Representations.  ZSTEMR
        /// computes eigenvalues by the dqds algorithm, while orthogonal
        /// eigenvectors are computed from various &quot;good&quot; L D L^T representations
        /// (also known as Relatively Robust Representations). Gram-Schmidt
        /// orthogonalization is avoided as far as possible. More specifically,
        /// the various steps of the algorithm are as follows.
        /// </para>
        /// <para>
        /// For each unreduced block (submatrix) of T,
        ///    (a) Compute T - sigma I  = L D L^T, so that L and D
        ///        define all the wanted eigenvalues to high relative accuracy.
        ///        This means that small relative changes in the entries of D and L
        ///        cause only small relative changes in the eigenvalues and
        ///        eigenvectors. The standard (unfactored) representation of the
        ///        tridiagonal matrix T does not have this property in general.
        ///    (b) Compute the eigenvalues to suitable accuracy.
        ///        If the eigenvectors are desired, the algorithm attains full
        ///        accuracy of the computed eigenvalues only right before
        ///        the corresponding vectors have to be computed, see steps c) and d).
        ///    (c) For each cluster of close eigenvalues, select a new
        ///        shift close to the cluster, find a new factorization, and refine
        ///        the shifted eigenvalues to suitable accuracy.
        ///    (d) For each eigenvalue with a large enough relative separation compute
        ///        the corresponding eigenvector by forming a rank revealing twisted
        ///        factorization. Go back to (c) for any clusters that remain.
        /// </para>
        /// <para>
        /// The desired accuracy of the output can be specified by the input
        /// parameter ABSTOL.
        /// </para>
        /// <para>
        /// For more details, see ZSTEMR&#39;s documentation and:
        /// - Inderjit S. Dhillon and Beresford N. Parlett: &quot;Multiple representations
        ///   to compute orthogonal eigenvectors of symmetric tridiagonal matrices,&quot;
        ///   Linear Algebra and its Applications, 387(1), pp. 1-28, August 2004.
        /// - Inderjit Dhillon and Beresford Parlett: &quot;Orthogonal Eigenvectors and
        ///   Relative Gaps,&quot; SIAM Journal on Matrix Analysis and Applications, Vol. 25,
        ///   2004.  Also LAPACK Working Note 154.
        /// - Inderjit Dhillon: &quot;A new O(n^2) algorithm for the symmetric
        ///   tridiagonal eigenvalue/eigenvector problem&quot;,
        ///   Computer Science Division Technical Report No. UCB/CSD-97-971,
        ///   UC Berkeley, May 1997.
        /// </para>
        /// <para>
        /// Note 1 : ZHEEVR_2STAGE calls ZSTEMR when the full spectrum is requested
        /// on machines which conform to the ieee-754 floating point standard.
        /// ZHEEVR_2STAGE calls DSTEBZ and ZSTEIN on non-ieee machines and
        /// when partial spectrum requests are made.
        /// </para>
        /// <para>
        /// Normal execution of ZSTEMR may create NaNs and infinities and
        /// hence may abort due to a floating point exception in environments
        /// which do not handle NaNs and infinities in the ieee standard default
        /// manner.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="jobz">
        /// [in] JOBZ is CHARACTER*1.
        /// = &#39;N&#39;:  Compute eigenvalues only;
        /// = &#39;V&#39;:  Compute eigenvalues and eigenvectors.
        /// Not available in this release.
        /// </param>
        /// <param name="range">
        /// [in] RANGE is CHARACTER*1.
        /// = &#39;A&#39;: all eigenvalues will be found.
        /// = &#39;V&#39;: all eigenvalues in the half-open interval (VL,VU]
        /// will be found.
        /// = &#39;I&#39;: the IL-th through IU-th eigenvalues will be found.
        /// For RANGE = &#39;V&#39; or &#39;I&#39; and IU - IL &lt; N - 1, DSTEBZ and
        /// ZSTEIN are called
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// = &#39;U&#39;:  Upper triangle of A is stored;
        /// = &#39;L&#39;:  Lower triangle of A is stored.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is COMPLEX*16 array, dimension (LDA, N).
        /// On entry, the Hermitian matrix A.  If UPLO = &#39;U&#39;, the
        /// leading N-by-N upper triangular part of A contains the
        /// upper triangular part of the matrix A.  If UPLO = &#39;L&#39;,
        /// the leading N-by-N lower triangular part of A contains
        /// the lower triangular part of the matrix A.
        /// On exit, the lower triangle (if UPLO=&#39;L&#39;) or the upper
        /// triangle (if UPLO=&#39;U&#39;) of A, including the diagonal, is
        /// destroyed.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="vl">
        /// [in] VL is DOUBLE PRECISION.
        /// If RANGE=&#39;V&#39;, the lower bound of the interval to
        /// be searched for eigenvalues. VL &lt; VU.
        /// Not referenced if RANGE = &#39;A&#39; or &#39;I&#39;.
        /// </param>
        /// <param name="vu">
        /// [in] VU is DOUBLE PRECISION.
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
        /// [in] ABSTOL is DOUBLE PRECISION.
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
        /// by reducing A to tridiagonal form.
        /// 
        /// See &quot;Computing Small Singular Values of Bidiagonal Matrices
        /// with Guaranteed High Relative Accuracy,&quot; by Demmel and
        /// Kahan, LAPACK Working Note #3.
        /// 
        /// If high relative accuracy is important, set ABSTOL to
        /// DLAMCH( &#39;Safe minimum&#39; ).  Doing so will guarantee that
        /// eigenvalues are computed to high relative accuracy when
        /// possible in future releases.  The current code does not
        /// make any guarantees about high relative accuracy, but
        /// future releases will. See J. Barlow and J. Demmel,
        /// &quot;Computing Accurate Eigensystems of Scaled Diagonally
        /// Dominant Matrices&quot;, LAPACK Working Note #7, for a discussion
        /// of which matrices define their eigenvalues to high relative
        /// accuracy.
        /// </param>
        /// <param name="m">
        /// [out] M is INTEGER.
        /// The total number of eigenvalues found.  0 &lt;= M &lt;= N.
        /// If RANGE = &#39;A&#39;, M = N, and if RANGE = &#39;I&#39;, M = IU-IL+1.
        /// </param>
        /// <param name="w">
        /// [out] W is DOUBLE PRECISION array, dimension (N).
        /// The first M elements contain the selected eigenvalues in
        /// ascending order.
        /// </param>
        /// <param name="z">
        /// [out] Z is COMPLEX*16 array, dimension (LDZ, max(1,M)).
        /// If JOBZ = &#39;V&#39;, then if INFO = 0, the first M columns of Z
        /// contain the orthonormal eigenvectors of the matrix A
        /// corresponding to the selected eigenvalues, with the i-th
        /// column of Z holding the eigenvector associated with W(i).
        /// If JOBZ = &#39;N&#39;, then Z is not referenced.
        /// Note: the user must ensure that at least max(1,M) columns are
        /// supplied in the array Z; if RANGE = &#39;V&#39;, the exact value of M
        /// is not known in advance and an upper bound must be used.
        /// </param>
        /// <param name="ldz">
        /// [in] LDZ is INTEGER.
        /// The leading dimension of the array Z.  LDZ &gt;= 1, and if
        /// JOBZ = &#39;V&#39;, LDZ &gt;= max(1,N).
        /// </param>
        /// <param name="isuppz">
        /// [out] ISUPPZ is INTEGER array, dimension ( 2*max(1,M) ).
        /// The support of the eigenvectors in Z, i.e., the indices
        /// indicating the nonzero elements in Z. The i-th eigenvector
        /// is nonzero only in elements ISUPPZ( 2*i-1 ) through
        /// ISUPPZ( 2*i ). This is an output of ZSTEMR (tridiagonal
        /// matrix). The support of the eigenvectors of A is typically
        /// 1:N because of the unitary transformations applied by ZUNMTR.
        /// Implemented only for RANGE = &#39;A&#39; or &#39;I&#39; and IU - IL = N - 1
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0:  Internal error
        /// </returns>
        /// <remarks>
        /// <para>
        ///  All details about the 2stage techniques are available in:
        /// </para>
        /// <para>
        ///  Azzam Haidar, Hatem Ltaief, and Jack Dongarra.
        ///  Parallel reduction to condensed forms for symmetric eigenvalue problems
        ///  using aggregated fine-grained and memory-aware kernels. In Proceedings
        ///  of 2011 International Conference for High Performance Computing,
        ///  Networking, Storage and Analysis (SC &#39;11), New York, NY, USA,
        ///  Article 8 , 11 pages.
        ///  http://doi.acm.org/10.1145/2063384.2063394
        /// </para>
        /// <para>
        ///  A. Haidar, J. Kurzak, P. Luszczek, 2013.
        ///  An improved parallel singular value algorithm and its implementation
        ///  for multicore hardware, In Proceedings of 2013 International Conference
        ///  for High Performance Computing, Networking, Storage and Analysis (SC &#39;13).
        ///  Denver, Colorado, USA, 2013.
        ///  Article 90, 12 pages.
        ///  http://doi.acm.org/10.1145/2503210.2503292
        /// </para>
        /// <para>
        ///  A. Haidar, R. Solca, S. Tomov, T. Schulthess and J. Dongarra.
        ///  A novel hybrid CPU-GPU generalized eigensolver for electronic structure
        ///  calculations based on fine-grained memory aware tasks.
        ///  International Journal of High Performance Computing Applications.
        ///  Volume 28 Issue 2, Pages 196-209, May 2014.
        ///  http://hpc.sagepub.com/content/28/2/196
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zheevr_2stage", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zheevr2stage(
            MatrixLayout matrixLayout,
            char jobz,
            char range,
            char uplo,
            int n,
            Complex* a,
            int lda,
            double vl,
            double vu,
            int il,
            int iu,
            double abstol,
            int* m,
            double* w,
            Complex* z,
            int ldz,
            int* isuppz);
    }
}
