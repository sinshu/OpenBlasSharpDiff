using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DGELSS computes the minimum norm solution to a real linear least
        /// squares problem:
        /// </para>
        /// <para>
        /// Minimize 2-norm(| b - A*x |).
        /// </para>
        /// <para>
        /// using the singular value decomposition (SVD) of A. A is an M-by-N
        /// matrix which may be rank-deficient.
        /// </para>
        /// <para>
        /// Several right hand side vectors b and solution vectors x can be
        /// handled in a single call; they are stored as the columns of the
        /// M-by-NRHS right hand side matrix B and the N-by-NRHS solution matrix
        /// X.
        /// </para>
        /// <para>
        /// The effective rank of A is determined by treating as zero those
        /// singular values which are less than RCOND times the largest singular
        /// value.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The number of rows of the matrix A. M &gt;= 0.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of columns of the matrix A. N &gt;= 0.
        /// </param>
        /// <param name="nrhs">
        /// [in] NRHS is INTEGER.
        /// The number of right hand sides, i.e., the number of columns
        /// of the matrices B and X. NRHS &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is DOUBLE PRECISION array, dimension (LDA,N).
        /// On entry, the M-by-N matrix A.
        /// On exit, the first min(m,n) rows of A are overwritten with
        /// its right singular vectors, stored rowwise.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,M).
        /// </param>
        /// <param name="b">
        /// [in,out] B is DOUBLE PRECISION array, dimension (LDB,NRHS).
        /// On entry, the M-by-NRHS right hand side matrix B.
        /// On exit, B is overwritten by the N-by-NRHS solution
        /// matrix X.  If m &gt;= n and RANK = n, the residual
        /// sum-of-squares for the solution in the i-th column is given
        /// by the sum of squares of elements n+1:m in that column.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B. LDB &gt;= max(1,max(M,N)).
        /// </param>
        /// <param name="s">
        /// [out] S is DOUBLE PRECISION array, dimension (min(M,N)).
        /// The singular values of A in decreasing order.
        /// The condition number of A in the 2-norm = S(1)/S(min(m,n)).
        /// </param>
        /// <param name="rcond">
        /// [in] RCOND is DOUBLE PRECISION.
        /// RCOND is used to determine the effective rank of A.
        /// Singular values S(i) &lt;= RCOND*S(1) are treated as zero.
        /// If RCOND &lt; 0, machine precision is used instead.
        /// </param>
        /// <param name="rank">
        /// [out] RANK is INTEGER.
        /// The effective rank of A, i.e., the number of singular values
        /// which are greater than RCOND*S(1).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value.
        /// &gt; 0:  the algorithm for computing the SVD failed to converge;
        /// if INFO = i, i off-diagonal elements of an intermediate
        /// bidiagonal form did not converge to zero.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dgelss", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dgelss(
            MatrixLayout matrixLayout,
            int m,
            int n,
            int nrhs,
            double* a,
            int lda,
            double* b,
            int ldb,
            double* s,
            double rcond,
            int* rank);
    }
}
