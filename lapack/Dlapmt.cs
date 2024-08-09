using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DLAPMT rearranges the columns of the M by N matrix X as specified
        /// by the permutation K(1),K(2),...,K(N) of the integers 1,...,N.
        /// If FORWRD = .TRUE.,  forward permutation:
        /// </para>
        /// <para>
        ///      X(*,K(J)) is moved X(*,J) for J = 1,2,...,N.
        /// </para>
        /// <para>
        /// If FORWRD = .FALSE., backward permutation:
        /// </para>
        /// <para>
        ///      X(*,J) is moved to X(*,K(J)) for J = 1,2,...,N.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="forwrd">
        /// [in] FORWRD is LOGICAL.
        /// = .TRUE., forward permutation
        /// = .FALSE., backward permutation
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The number of rows of the matrix X. M &gt;= 0.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of columns of the matrix X. N &gt;= 0.
        /// </param>
        /// <param name="x">
        /// [in,out] X is DOUBLE PRECISION array, dimension (LDX,N).
        /// On entry, the M by N matrix X.
        /// On exit, X contains the permuted matrix X.
        /// </param>
        /// <param name="ldx">
        /// [in] LDX is INTEGER.
        /// The leading dimension of the array X, LDX &gt;= MAX(1,M).
        /// </param>
        /// <param name="k">
        /// [in,out] K is INTEGER array, dimension (N).
        /// On entry, K contains the permutation vector. K is used as
        /// internal workspace, but reset to its original value on
        /// output.
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dlapmt", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dlapmt(
            MatrixLayout matrixLayout,
            bool forwrd,
            int m,
            int n,
            double* x,
            int ldx,
            int* k);
    }
}
