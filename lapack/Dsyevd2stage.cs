using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DSYEVD_2STAGE computes all eigenvalues and, optionally, eigenvectors of a
        /// real symmetric matrix A using the 2stage technique for
        /// the reduction to tridiagonal. If eigenvectors are desired, it uses a
        /// divide and conquer algorithm.
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
        /// [in,out] A is DOUBLE PRECISION array, dimension (LDA, N).
        /// On entry, the symmetric matrix A.  If UPLO = &#39;U&#39;, the
        /// leading N-by-N upper triangular part of A contains the
        /// upper triangular part of the matrix A.  If UPLO = &#39;L&#39;,
        /// the leading N-by-N lower triangular part of A contains
        /// the lower triangular part of the matrix A.
        /// On exit, if JOBZ = &#39;V&#39;, then if INFO = 0, A contains the
        /// orthonormal eigenvectors of the matrix A.
        /// If JOBZ = &#39;N&#39;, then on exit the lower triangle (if UPLO=&#39;L&#39;)
        /// or the upper triangle (if UPLO=&#39;U&#39;) of A, including the
        /// diagonal, is destroyed.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="w">
        /// [out] W is DOUBLE PRECISION array, dimension (N).
        /// If INFO = 0, the eigenvalues in ascending order.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0:  if INFO = i and JOBZ = &#39;N&#39;, then the algorithm failed
        /// to converge; i off-diagonal elements of an intermediate
        /// tridiagonal form did not converge to zero;
        /// if INFO = i and JOBZ = &#39;V&#39;, then the algorithm failed
        /// to compute an eigenvalue while working on the submatrix
        /// lying in rows and columns INFO/(N+1) through
        /// mod(INFO,N+1).
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
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dsyevd_2stage", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dsyevd2stage(
            MatrixLayout matrixLayout,
            char jobz,
            char uplo,
            int n,
            double* a,
            int lda,
            double* w);
    }
}
