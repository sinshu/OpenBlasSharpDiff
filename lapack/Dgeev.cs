using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DGEEV computes for an N-by-N real nonsymmetric matrix A, the
        /// eigenvalues and, optionally, the left and/or right eigenvectors.
        /// </para>
        /// <para>
        /// The right eigenvector v(j) of A satisfies
        ///                  A * v(j) = lambda(j) * v(j)
        /// where lambda(j) is its eigenvalue.
        /// The left eigenvector u(j) of A satisfies
        ///               u(j)**H * A = lambda(j) * u(j)**H
        /// where u(j)**H denotes the conjugate-transpose of u(j).
        /// </para>
        /// <para>
        /// The computed eigenvectors are normalized to have Euclidean norm
        /// equal to 1 and largest component real.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="jobvl">
        /// [in] JOBVL is CHARACTER*1.
        /// = &#39;N&#39;: left eigenvectors of A are not computed;
        /// = &#39;V&#39;: left eigenvectors of A are computed.
        /// </param>
        /// <param name="jobvr">
        /// [in] JOBVR is CHARACTER*1.
        /// = &#39;N&#39;: right eigenvectors of A are not computed;
        /// = &#39;V&#39;: right eigenvectors of A are computed.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix A. N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is DOUBLE PRECISION array, dimension (LDA,N).
        /// On entry, the N-by-N matrix A.
        /// On exit, A has been overwritten.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="wr">
        /// [out] WR is DOUBLE PRECISION array, dimension (N).
        /// </param>
        /// <param name="wi">
        /// [out] WI is DOUBLE PRECISION array, dimension (N).
        /// WR and WI contain the real and imaginary parts,
        /// respectively, of the computed eigenvalues.  Complex
        /// conjugate pairs of eigenvalues appear consecutively
        /// with the eigenvalue having the positive imaginary part
        /// first.
        /// </param>
        /// <param name="vl">
        /// [out] VL is DOUBLE PRECISION array, dimension (LDVL,N).
        /// If JOBVL = &#39;V&#39;, the left eigenvectors u(j) are stored one
        /// after another in the columns of VL, in the same order
        /// as their eigenvalues.
        /// If JOBVL = &#39;N&#39;, VL is not referenced.
        /// If the j-th eigenvalue is real, then u(j) = VL(:,j),
        /// the j-th column of VL.
        /// If the j-th and (j+1)-st eigenvalues form a complex
        /// conjugate pair, then u(j) = VL(:,j) + i*VL(:,j+1) and
        /// u(j+1) = VL(:,j) - i*VL(:,j+1).
        /// </param>
        /// <param name="ldvl">
        /// [in] LDVL is INTEGER.
        /// The leading dimension of the array VL.  LDVL &gt;= 1; if
        /// JOBVL = &#39;V&#39;, LDVL &gt;= N.
        /// </param>
        /// <param name="vr">
        /// [out] VR is DOUBLE PRECISION array, dimension (LDVR,N).
        /// If JOBVR = &#39;V&#39;, the right eigenvectors v(j) are stored one
        /// after another in the columns of VR, in the same order
        /// as their eigenvalues.
        /// If JOBVR = &#39;N&#39;, VR is not referenced.
        /// If the j-th eigenvalue is real, then v(j) = VR(:,j),
        /// the j-th column of VR.
        /// If the j-th and (j+1)-st eigenvalues form a complex
        /// conjugate pair, then v(j) = VR(:,j) + i*VR(:,j+1) and
        /// v(j+1) = VR(:,j) - i*VR(:,j+1).
        /// </param>
        /// <param name="ldvr">
        /// [in] LDVR is INTEGER.
        /// The leading dimension of the array VR.  LDVR &gt;= 1; if
        /// JOBVR = &#39;V&#39;, LDVR &gt;= N.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value.
        /// &gt; 0:  if INFO = i, the QR algorithm failed to compute all the
        /// eigenvalues, and no eigenvectors have been computed;
        /// elements i+1:N of WR and WI contain eigenvalues which
        /// have converged.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dgeev", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dgeev(
            MatrixLayout matrixLayout,
            char jobvl,
            char jobvr,
            int n,
            double* a,
            int lda,
            double* wr,
            double* wi,
            double* vl,
            int ldvl,
            double* vr,
            int ldvr);
    }
}
