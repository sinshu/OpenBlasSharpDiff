using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DSBGVX computes selected eigenvalues, and optionally, eigenvectors
        /// of a real generalized symmetric-definite banded eigenproblem, of
        /// the form A*x=(lambda)*B*x.  Here A and B are assumed to be symmetric
        /// and banded, and B is also positive definite.  Eigenvalues and
        /// eigenvectors can be selected by specifying either all eigenvalues,
        /// a range of values or a range of indices for the desired eigenvalues.
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
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// = &#39;U&#39;:  Upper triangles of A and B are stored;
        /// = &#39;L&#39;:  Lower triangles of A and B are stored.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrices A and B.  N &gt;= 0.
        /// </param>
        /// <param name="ka">
        /// [in] KA is INTEGER.
        /// The number of superdiagonals of the matrix A if UPLO = &#39;U&#39;,
        /// or the number of subdiagonals if UPLO = &#39;L&#39;.  KA &gt;= 0.
        /// </param>
        /// <param name="kb">
        /// [in] KB is INTEGER.
        /// The number of superdiagonals of the matrix B if UPLO = &#39;U&#39;,
        /// or the number of subdiagonals if UPLO = &#39;L&#39;.  KB &gt;= 0.
        /// </param>
        /// <param name="ab">
        /// [in,out] AB is DOUBLE PRECISION array, dimension (LDAB, N).
        /// On entry, the upper or lower triangle of the symmetric band
        /// matrix A, stored in the first ka+1 rows of the array.  The
        /// j-th column of A is stored in the j-th column of the array AB
        /// as follows:
        /// if UPLO = &#39;U&#39;, AB(ka+1+i-j,j) = A(i,j) for max(1,j-ka)&lt;=i&lt;=j;
        /// if UPLO = &#39;L&#39;, AB(1+i-j,j)    = A(i,j) for j&lt;=i&lt;=min(n,j+ka).
        /// 
        /// On exit, the contents of AB are destroyed.
        /// </param>
        /// <param name="ldab">
        /// [in] LDAB is INTEGER.
        /// The leading dimension of the array AB.  LDAB &gt;= KA+1.
        /// </param>
        /// <param name="bb">
        /// [in,out] BB is DOUBLE PRECISION array, dimension (LDBB, N).
        /// On entry, the upper or lower triangle of the symmetric band
        /// matrix B, stored in the first kb+1 rows of the array.  The
        /// j-th column of B is stored in the j-th column of the array BB
        /// as follows:
        /// if UPLO = &#39;U&#39;, BB(ka+1+i-j,j) = B(i,j) for max(1,j-kb)&lt;=i&lt;=j;
        /// if UPLO = &#39;L&#39;, BB(1+i-j,j)    = B(i,j) for j&lt;=i&lt;=min(n,j+kb).
        /// 
        /// On exit, the factor S from the split Cholesky factorization
        /// B = S**T*S, as returned by DPBSTF.
        /// </param>
        /// <param name="ldbb">
        /// [in] LDBB is INTEGER.
        /// The leading dimension of the array BB.  LDBB &gt;= KB+1.
        /// </param>
        /// <param name="q">
        /// [out] Q is DOUBLE PRECISION array, dimension (LDQ, N).
        /// If JOBZ = &#39;V&#39;, the n-by-n matrix used in the reduction of
        /// A*x = (lambda)*B*x to standard form, i.e. C*x = (lambda)*x,
        /// and consequently C to tridiagonal form.
        /// If JOBZ = &#39;N&#39;, the array Q is not referenced.
        /// </param>
        /// <param name="ldq">
        /// [in] LDQ is INTEGER.
        /// The leading dimension of the array Q.  If JOBZ = &#39;N&#39;,
        /// LDQ &gt;= 1. If JOBZ = &#39;V&#39;, LDQ &gt;= max(1,N).
        /// </param>
        /// <param name="vl">
        /// [in] VL is DOUBLE PRECISION.
        /// 
        /// If RANGE=&#39;V&#39;, the lower bound of the interval to
        /// be searched for eigenvalues. VL &lt; VU.
        /// Not referenced if RANGE = &#39;A&#39; or &#39;I&#39;.
        /// </param>
        /// <param name="vu">
        /// [in] VU is DOUBLE PRECISION.
        /// 
        /// If RANGE=&#39;V&#39;, the upper bound of the interval to
        /// be searched for eigenvalues. VL &lt; VU.
        /// Not referenced if RANGE = &#39;A&#39; or &#39;I&#39;.
        /// </param>
        /// <param name="il">
        /// [in] IL is INTEGER.
        /// 
        /// If RANGE=&#39;I&#39;, the index of the
        /// smallest eigenvalue to be returned.
        /// 1 &lt;= IL &lt;= IU &lt;= N, if N &gt; 0; IL = 1 and IU = 0 if N = 0.
        /// Not referenced if RANGE = &#39;A&#39; or &#39;V&#39;.
        /// </param>
        /// <param name="iu">
        /// [in] IU is INTEGER.
        /// 
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
        /// Eigenvalues will be computed most accurately when ABSTOL is
        /// set to twice the underflow threshold 2*DLAMCH(&#39;S&#39;), not zero.
        /// If this routine returns with INFO&gt;0, indicating that some
        /// eigenvectors did not converge, try setting ABSTOL to
        /// 2*DLAMCH(&#39;S&#39;).
        /// </param>
        /// <param name="m">
        /// [out] M is INTEGER.
        /// The total number of eigenvalues found.  0 &lt;= M &lt;= N.
        /// If RANGE = &#39;A&#39;, M = N, and if RANGE = &#39;I&#39;, M = IU-IL+1.
        /// </param>
        /// <param name="w">
        /// [out] W is DOUBLE PRECISION array, dimension (N).
        /// If INFO = 0, the eigenvalues in ascending order.
        /// </param>
        /// <param name="z">
        /// [out] Z is DOUBLE PRECISION array, dimension (LDZ, N).
        /// If JOBZ = &#39;V&#39;, then if INFO = 0, Z contains the matrix Z of
        /// eigenvectors, with the i-th column of Z holding the
        /// eigenvector associated with W(i).  The eigenvectors are
        /// normalized so Z**T*B*Z = I.
        /// If JOBZ = &#39;N&#39;, then Z is not referenced.
        /// </param>
        /// <param name="ldz">
        /// [in] LDZ is INTEGER.
        /// The leading dimension of the array Z.  LDZ &gt;= 1, and if
        /// JOBZ = &#39;V&#39;, LDZ &gt;= max(1,N).
        /// </param>
        /// <param name="ifail">
        /// [out] IFAIL is INTEGER array, dimension (M).
        /// If JOBZ = &#39;V&#39;, then if INFO = 0, the first M elements of
        /// IFAIL are zero.  If INFO &gt; 0, then IFAIL contains the
        /// indices of the eigenvalues that failed to converge.
        /// If JOBZ = &#39;N&#39;, then IFAIL is not referenced.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// &lt;= N: if INFO = i, then i eigenvectors failed to converge.
        /// Their indices are stored in IFAIL.
        /// &gt; N:  DPBSTF returned an error code; i.e.,
        /// if INFO = N + i, for 1 &lt;= i &lt;= N, then the leading
        /// principal minor of order i of B is not positive.
        /// The factorization of B could not be completed and
        /// no eigenvalues or eigenvectors were computed.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dsbgvx", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dsbgvx(
            MatrixLayout matrixLayout,
            char jobz,
            char range,
            char uplo,
            int n,
            int ka,
            int kb,
            double* ab,
            int ldab,
            double* bb,
            int ldbb,
            double* q,
            int ldq,
            double vl,
            double vu,
            int il,
            int iu,
            double abstol,
            int* m,
            double* w,
            double* z,
            int ldz,
            int* ifail);
    }
}
