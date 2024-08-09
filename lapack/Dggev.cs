using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DGGEV computes for a pair of N-by-N real nonsymmetric matrices (A,B)
        /// the generalized eigenvalues, and optionally, the left and/or right
        /// generalized eigenvectors.
        /// </para>
        /// <para>
        /// A generalized eigenvalue for a pair of matrices (A,B) is a scalar
        /// lambda or a ratio alpha/beta = lambda, such that A - lambda*B is
        /// singular. It is usually represented as the pair (alpha,beta), as
        /// there is a reasonable interpretation for beta=0, and even for both
        /// being zero.
        /// </para>
        /// <para>
        /// The right eigenvector v(j) corresponding to the eigenvalue lambda(j)
        /// of (A,B) satisfies
        /// </para>
        /// <para>
        ///                  A * v(j) = lambda(j) * B * v(j).
        /// </para>
        /// <para>
        /// The left eigenvector u(j) corresponding to the eigenvalue lambda(j)
        /// of (A,B) satisfies
        /// </para>
        /// <para>
        ///                  u(j)**H * A  = lambda(j) * u(j)**H * B .
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
        /// [in,out] A is DOUBLE PRECISION array, dimension (LDA, N).
        /// On entry, the matrix A in the pair (A,B).
        /// On exit, A has been overwritten.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="b">
        /// [in,out] B is DOUBLE PRECISION array, dimension (LDB, N).
        /// On entry, the matrix B in the pair (A,B).
        /// On exit, B has been overwritten.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of B.  LDB &gt;= max(1,N).
        /// </param>
        /// <param name="alphar">
        /// [out] ALPHAR is DOUBLE PRECISION array, dimension (N).
        /// </param>
        /// <param name="alphai">
        /// [out] ALPHAI is DOUBLE PRECISION array, dimension (N).
        /// </param>
        /// <param name="beta">
        /// [out] BETA is DOUBLE PRECISION array, dimension (N).
        /// On exit, (ALPHAR(j) + ALPHAI(j)*i)/BETA(j), j=1,...,N, will
        /// be the generalized eigenvalues.  If ALPHAI(j) is zero, then
        /// the j-th eigenvalue is real; if positive, then the j-th and
        /// (j+1)-st eigenvalues are a complex conjugate pair, with
        /// ALPHAI(j+1) negative.
        /// 
        /// Note: the quotients ALPHAR(j)/BETA(j) and ALPHAI(j)/BETA(j)
        /// may easily over- or underflow, and BETA(j) may even be zero.
        /// Thus, the user should avoid naively computing the ratio
        /// alpha/beta.  However, ALPHAR and ALPHAI will be always less
        /// than and usually comparable with norm(A) in magnitude, and
        /// BETA always less than and usually comparable with norm(B).
        /// </param>
        /// <param name="vl">
        /// [out] VL is DOUBLE PRECISION array, dimension (LDVL,N).
        /// If JOBVL = &#39;V&#39;, the left eigenvectors u(j) are stored one
        /// after another in the columns of VL, in the same order as
        /// their eigenvalues. If the j-th eigenvalue is real, then
        /// u(j) = VL(:,j), the j-th column of VL. If the j-th and
        /// (j+1)-th eigenvalues form a complex conjugate pair, then
        /// u(j) = VL(:,j)+i*VL(:,j+1) and u(j+1) = VL(:,j)-i*VL(:,j+1).
        /// Each eigenvector is scaled so the largest component has
        /// abs(real part)+abs(imag. part)=1.
        /// Not referenced if JOBVL = &#39;N&#39;.
        /// </param>
        /// <param name="ldvl">
        /// [in] LDVL is INTEGER.
        /// The leading dimension of the matrix VL. LDVL &gt;= 1, and
        /// if JOBVL = &#39;V&#39;, LDVL &gt;= N.
        /// </param>
        /// <param name="vr">
        /// [out] VR is DOUBLE PRECISION array, dimension (LDVR,N).
        /// If JOBVR = &#39;V&#39;, the right eigenvectors v(j) are stored one
        /// after another in the columns of VR, in the same order as
        /// their eigenvalues. If the j-th eigenvalue is real, then
        /// v(j) = VR(:,j), the j-th column of VR. If the j-th and
        /// (j+1)-th eigenvalues form a complex conjugate pair, then
        /// v(j) = VR(:,j)+i*VR(:,j+1) and v(j+1) = VR(:,j)-i*VR(:,j+1).
        /// Each eigenvector is scaled so the largest component has
        /// abs(real part)+abs(imag. part)=1.
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
        /// = 1,...,N:
        /// The QZ iteration failed.  No eigenvectors have been
        /// calculated, but ALPHAR(j), ALPHAI(j), and BETA(j)
        /// should be correct for j=INFO+1,...,N.
        /// &gt; N:  =N+1: other than QZ iteration failed in DHGEQZ.
        /// =N+2: error return from DTGEVC.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dggev", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dggev(
            MatrixLayout matrixLayout,
            char jobvl,
            char jobvr,
            int n,
            double* a,
            int lda,
            double* b,
            int ldb,
            double* alphar,
            double* alphai,
            double* beta,
            double* vl,
            int ldvl,
            double* vr,
            int ldvr);
    }
}
