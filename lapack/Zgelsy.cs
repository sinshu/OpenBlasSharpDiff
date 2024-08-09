using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZGELSY computes the minimum-norm solution to a complex linear least
        /// squares problem:
        ///     minimize || A * X - B ||
        /// using a complete orthogonal factorization of A.  A is an M-by-N
        /// matrix which may be rank-deficient.
        /// </para>
        /// <para>
        /// Several right hand side vectors b and solution vectors x can be
        /// handled in a single call; they are stored as the columns of the
        /// M-by-NRHS right hand side matrix B and the N-by-NRHS solution
        /// matrix X.
        /// </para>
        /// <para>
        /// The routine first computes a QR factorization with column pivoting:
        ///     A * P = Q * [ R11 R12 ]
        ///                 [  0  R22 ]
        /// with R11 defined as the largest leading submatrix whose estimated
        /// condition number is less than 1/RCOND.  The order of R11, RANK,
        /// is the effective rank of A.
        /// </para>
        /// <para>
        /// Then, R22 is considered to be negligible, and R12 is annihilated
        /// by unitary transformations from the right, arriving at the
        /// complete orthogonal factorization:
        ///    A * P = Q * [ T11 0 ] * Z
        ///                [  0  0 ]
        /// The minimum-norm solution is then
        ///    X = P * Z**H [ inv(T11)*Q1**H*B ]
        ///                 [        0         ]
        /// where Q1 consists of the first RANK columns of Q.
        /// </para>
        /// <para>
        /// This routine is basically identical to the original xGELSX except
        /// three differences:
        ///   o The permutation of matrix B (the right hand side) is faster and
        ///     more simple.
        ///   o The call to the subroutine xGEQPF has been substituted by the
        ///     the call to the subroutine xGEQP3. This subroutine is a Blas-3
        ///     version of the QR factorization with column pivoting.
        ///   o Matrix B (the right hand side) is updated with Blas-3.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The number of rows of the matrix A.  M &gt;= 0.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of columns of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="nrhs">
        /// [in] NRHS is INTEGER.
        /// The number of right hand sides, i.e., the number of
        /// columns of matrices B and X. NRHS &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is COMPLEX*16 array, dimension (LDA,N).
        /// On entry, the M-by-N matrix A.
        /// On exit, A has been overwritten by details of its
        /// complete orthogonal factorization.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,M).
        /// </param>
        /// <param name="b">
        /// [in,out] B is COMPLEX*16 array, dimension (LDB,NRHS).
        /// On entry, the M-by-NRHS right hand side matrix B.
        /// On exit, the N-by-NRHS solution matrix X.
        /// If M = 0 or N = 0, B is not referenced.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B. LDB &gt;= max(1,M,N).
        /// </param>
        /// <param name="jpvt">
        /// [in,out] JPVT is INTEGER array, dimension (N).
        /// On entry, if JPVT(i) .ne. 0, the i-th column of A is permuted
        /// to the front of AP, otherwise column i is a free column.
        /// On exit, if JPVT(i) = k, then the i-th column of A*P
        /// was the k-th column of A.
        /// </param>
        /// <param name="rcond">
        /// [in] RCOND is DOUBLE PRECISION.
        /// RCOND is used to determine the effective rank of A, which
        /// is defined as the order of the largest leading triangular
        /// submatrix R11 in the QR factorization with pivoting of A,
        /// whose estimated condition number &lt; 1/RCOND.
        /// </param>
        /// <param name="rank">
        /// [out] RANK is INTEGER.
        /// The effective rank of A, i.e., the order of the submatrix
        /// R11.  This is the same as the order of the submatrix T11
        /// in the complete orthogonal factorization of A.
        /// If NRHS = 0, RANK = 0 on output.
        /// </param>
        /// <returns>
        /// = 0: successful exit
        /// &lt; 0: if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zgelsy", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zgelsy(
            MatrixLayout matrixLayout,
            int m,
            int n,
            int nrhs,
            Complex* a,
            int lda,
            Complex* b,
            int ldb,
            int* jpvt,
            double rcond,
            int* rank);
    }
}
