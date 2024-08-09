using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// SSTEVX computes selected eigenvalues and, optionally, eigenvectors
        /// of a real symmetric tridiagonal matrix A.  Eigenvalues and
        /// eigenvectors can be selected by specifying either a range of values
        /// or a range of indices for the desired eigenvalues.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
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
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix.  N &gt;= 0.
        /// </param>
        /// <param name="d">
        /// [in,out] D is REAL array, dimension (N).
        /// On entry, the n diagonal elements of the tridiagonal matrix
        /// A.
        /// On exit, D may be multiplied by a constant factor chosen
        /// to avoid over/underflow in computing the eigenvalues.
        /// </param>
        /// <param name="e">
        /// [in,out] E is REAL array, dimension (max(1,N-1)).
        /// On entry, the (n-1) subdiagonal elements of the tridiagonal
        /// matrix A in elements 1 to N-1 of E.
        /// On exit, E may be multiplied by a constant factor chosen
        /// to avoid over/underflow in computing the eigenvalues.
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
        /// where EPS is the machine precision.  If ABSTOL is less
        /// than or equal to zero, then  EPS*|T|  will be used in
        /// its place, where |T| is the 1-norm of the tridiagonal
        /// matrix.
        /// 
        /// Eigenvalues will be computed most accurately when ABSTOL is
        /// set to twice the underflow threshold 2*SLAMCH(&#39;S&#39;), not zero.
        /// If this routine returns with INFO&gt;0, indicating that some
        /// eigenvectors did not converge, try setting ABSTOL to
        /// 2*SLAMCH(&#39;S&#39;).
        /// 
        /// See &quot;Computing Small Singular Values of Bidiagonal Matrices
        /// with Guaranteed High Relative Accuracy,&quot; by Demmel and
        /// Kahan, LAPACK Working Note #3.
        /// </param>
        /// <param name="m">
        /// [out] M is INTEGER.
        /// The total number of eigenvalues found.  0 &lt;= M &lt;= N.
        /// If RANGE = &#39;A&#39;, M = N, and if RANGE = &#39;I&#39;, M = IU-IL+1.
        /// </param>
        /// <param name="w">
        /// [out] W is REAL array, dimension (N).
        /// The first M elements contain the selected eigenvalues in
        /// ascending order.
        /// </param>
        /// <param name="z">
        /// [out] Z is REAL array, dimension (LDZ, max(1,M) ).
        /// If JOBZ = &#39;V&#39;, then if INFO = 0, the first M columns of Z
        /// contain the orthonormal eigenvectors of the matrix A
        /// corresponding to the selected eigenvalues, with the i-th
        /// column of Z holding the eigenvector associated with W(i).
        /// If an eigenvector fails to converge (INFO &gt; 0), then that
        /// column of Z contains the latest approximation to the
        /// eigenvector, and the index of the eigenvector is returned
        /// in IFAIL.  If JOBZ = &#39;N&#39;, then Z is not referenced.
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
        /// &gt; 0:  if INFO = i, then i eigenvectors failed to converge.
        /// Their indices are stored in array IFAIL.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_sstevx", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Sstevx(
            MatrixLayout matrixLayout,
            char jobz,
            char range,
            int n,
            float* d,
            float* e,
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
