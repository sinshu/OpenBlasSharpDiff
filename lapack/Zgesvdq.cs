using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZCGESVDQ computes the singular value decomposition (SVD) of a complex
        /// M-by-N matrix A, where M &gt;= N. The SVD of A is written as
        ///                                    [++]   [xx]   [x0]   [xx]
        ///              A = U * SIGMA * V^*,  [++] = [xx] * [ox] * [xx]
        ///                                    [++]   [xx]
        /// where SIGMA is an N-by-N diagonal matrix, U is an M-by-N orthonormal
        /// matrix, and V is an N-by-N unitary matrix. The diagonal elements
        /// of SIGMA are the singular values of A. The columns of U and V are the
        /// left and the right singular vectors of A, respectively.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="joba">
        /// No description available.
        /// </param>
        /// <param name="jobp">
        /// No description available.
        /// </param>
        /// <param name="jobr">
        /// No description available.
        /// </param>
        /// <param name="jobu">
        /// No description available.
        /// </param>
        /// <param name="jobv">
        /// No description available.
        /// </param>
        /// <param name="m">
        /// No description available.
        /// </param>
        /// <param name="n">
        /// No description available.
        /// </param>
        /// <param name="a">
        /// No description available.
        /// </param>
        /// <param name="lda">
        /// No description available.
        /// </param>
        /// <param name="s">
        /// No description available.
        /// </param>
        /// <param name="u">
        /// No description available.
        /// </param>
        /// <param name="ldu">
        /// No description available.
        /// </param>
        /// <param name="v">
        /// No description available.
        /// </param>
        /// <param name="ldv">
        /// No description available.
        /// </param>
        /// <param name="numrank">
        /// No description available.
        /// </param>
        /// <remarks>
        /// <para>
        ///   1. The data movement (matrix transpose) is coded using simple nested
        ///   DO-loops because BLAS and LAPACK do not provide corresponding subroutines.
        ///   Those DO-loops are easily identified in this source code - by the CONTINUE
        ///   statements labeled with 11**. In an optimized version of this code, the
        ///   nested DO loops should be replaced with calls to an optimized subroutine.
        ///   2. This code scales A by 1/SQRT(M) if the largest ABS(A(i,j)) could cause
        ///   column norm overflow. This is the minial precaution and it is left to the
        ///   SVD routine (CGESVD) to do its own preemptive scaling if potential over-
        ///   or underflows are detected. To avoid repeated scanning of the array A,
        ///   an optimal implementation would do all necessary scaling before calling
        ///   CGESVD and the scaling in CGESVD can be switched off.
        ///   3. Other comments related to code optimization are given in comments in the
        ///   code, enclosed in [[double brackets]].
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zgesvdq", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zgesvdq(
            MatrixLayout matrixLayout,
            char joba,
            char jobp,
            char jobr,
            char jobu,
            char jobv,
            int m,
            int n,
            Complex* a,
            int lda,
            double* s,
            Complex* u,
            int ldu,
            Complex* v,
            int ldv,
            int* numrank);
    }
}
