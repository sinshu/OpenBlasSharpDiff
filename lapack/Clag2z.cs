using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CLAG2Z converts a COMPLEX matrix, SA, to a COMPLEX*16 matrix, A.
        /// </para>
        /// <para>
        /// Note that while it is possible to overflow while converting
        /// from double to single, it is not possible to overflow when
        /// converting from single to double.
        /// </para>
        /// <para>
        /// This is an auxiliary routine so there is no argument checking.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The number of lines of the matrix A.  M &gt;= 0.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of columns of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="sa">
        /// [in] SA is COMPLEX array, dimension (LDSA,N).
        /// On entry, the M-by-N coefficient matrix SA.
        /// </param>
        /// <param name="ldsa">
        /// [in] LDSA is INTEGER.
        /// The leading dimension of the array SA.  LDSA &gt;= max(1,M).
        /// </param>
        /// <param name="a">
        /// [out] A is COMPLEX*16 array, dimension (LDA,N).
        /// On exit, the M-by-N coefficient matrix A.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,M).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_clag2z", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Clag2z(
            MatrixLayout matrixLayout,
            int m,
            int n,
            Complex32* sa,
            int ldsa,
            Complex* a,
            int lda);
    }
}
