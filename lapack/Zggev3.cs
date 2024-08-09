using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZGGEV3 computes for a pair of N-by-N complex nonsymmetric matrices
        /// (A,B), the generalized eigenvalues, and optionally, the left and/or
        /// right generalized eigenvectors.
        /// </para>
        /// <para>
        /// A generalized eigenvalue for a pair of matrices (A,B) is a scalar
        /// lambda or a ratio alpha/beta = lambda, such that A - lambda*B is
        /// singular. It is usually represented as the pair (alpha,beta), as
        /// there is a reasonable interpretation for beta=0, and even for both
        /// being zero.
        /// </para>
        /// <para>
        /// The right generalized eigenvector v(j) corresponding to the
        /// generalized eigenvalue lambda(j) of (A,B) satisfies
        /// </para>
        /// <para>
        ///              A * v(j) = lambda(j) * B * v(j).
        /// </para>
        /// <para>
        /// The left generalized eigenvector u(j) corresponding to the
        /// generalized eigenvalues lambda(j) of (A,B) satisfies
        /// </para>
        /// <para>
        ///              u(j)**H * A = lambda(j) * u(j)**H * B
        /// </para>
        /// <para>
        /// where u(j)**H is the conjugate-transpose of u(j).
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="jobvl">
        /// [in] JOBVL is CHARACTER*1.
        /// = &#39;N&#39;:  do not compute the left generalized eigenvectors;
        /// = &#39;V&#39;:  compute the left generalized eigenvectors.
        /// </param>
        /// <param name="jobvr">
        /// [in] JOBVR is CHARACTER*1.
        /// = &#39;N&#39;:  do not compute the right generalized eigenvectors;
        /// = &#39;V&#39;:  compute the right generalized eigenvectors.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrices A, B, VL, and VR.  N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is COMPLEX*16 array, dimension (LDA, N).
        /// On entry, the matrix A in the pair (A,B).
        /// On exit, A has been overwritten.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="b">
        /// [in,out] B is COMPLEX*16 array, dimension (LDB, N).
        /// On entry, the matrix B in the pair (A,B).
        /// On exit, B has been overwritten.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of B.  LDB &gt;= max(1,N).
        /// </param>
        /// <param name="alpha">
        /// [out] ALPHA is COMPLEX*16 array, dimension (N).
        /// </param>
        /// <param name="beta">
        /// [out] BETA is COMPLEX*16 array, dimension (N).
        /// On exit, ALPHA(j)/BETA(j), j=1,...,N, will be the
        /// generalized eigenvalues.
        /// 
        /// Note: the quotients ALPHA(j)/BETA(j) may easily over- or
        /// underflow, and BETA(j) may even be zero.  Thus, the user
        /// should avoid naively computing the ratio alpha/beta.
        /// However, ALPHA will be always less than and usually
        /// comparable with norm(A) in magnitude, and BETA always less
        /// than and usually comparable with norm(B).
        /// </param>
        /// <param name="vl">
        /// [out] VL is COMPLEX*16 array, dimension (LDVL,N).
        /// If JOBVL = &#39;V&#39;, the left generalized eigenvectors u(j) are
        /// stored one after another in the columns of VL, in the same
        /// order as their eigenvalues.
        /// Each eigenvector is scaled so the largest component has
        /// abs(real part) + abs(imag. part) = 1.
        /// Not referenced if JOBVL = &#39;N&#39;.
        /// </param>
        /// <param name="ldvl">
        /// [in] LDVL is INTEGER.
        /// The leading dimension of the matrix VL. LDVL &gt;= 1, and
        /// if JOBVL = &#39;V&#39;, LDVL &gt;= N.
        /// </param>
        /// <param name="vr">
        /// [out] VR is COMPLEX*16 array, dimension (LDVR,N).
        /// If JOBVR = &#39;V&#39;, the right generalized eigenvectors v(j) are
        /// stored one after another in the columns of VR, in the same
        /// order as their eigenvalues.
        /// Each eigenvector is scaled so the largest component has
        /// abs(real part) + abs(imag. part) = 1.
        /// Not referenced if JOBVR = &#39;N&#39;.
        /// </param>
        /// <param name="ldvr">
        /// [in] LDVR is INTEGER.
        /// The leading dimension of the matrix VR. LDVR &gt;= 1, and
        /// if JOBVR = &#39;V&#39;, LDVR &gt;= N.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value.
        /// =1,...,N:
        /// The QZ iteration failed.  No eigenvectors have been
        /// calculated, but ALPHA(j) and BETA(j) should be
        /// correct for j=INFO+1,...,N.
        /// &gt; N:  =N+1: other then QZ iteration failed in ZHGEQZ,
        /// =N+2: error return from ZTGEVC.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zggev3", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zggev3(
            MatrixLayout matrixLayout,
            char jobvl,
            char jobvr,
            int n,
            Complex* a,
            int lda,
            Complex* b,
            int ldb,
            Complex* alpha,
            Complex* beta,
            Complex* vl,
            int ldvl,
            Complex* vr,
            int ldvr);
    }
}
