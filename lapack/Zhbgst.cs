using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZHBGST reduces a complex Hermitian-definite banded generalized
        /// eigenproblem  A*x = lambda*B*x  to standard form  C*y = lambda*y,
        /// such that C has the same bandwidth as A.
        /// </para>
        /// <para>
        /// B must have been previously factorized as S**H*S by ZPBSTF, using a
        /// split Cholesky factorization. A is overwritten by C = X**H*A*X, where
        /// X = S**(-1)*Q and Q is a unitary matrix chosen to preserve the
        /// bandwidth of A.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="vect">
        /// [in] VECT is CHARACTER*1.
        /// = &#39;N&#39;:  do not form the transformation matrix X;
        /// = &#39;V&#39;:  form X.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// = &#39;U&#39;:  Upper triangle of A is stored;
        /// = &#39;L&#39;:  Lower triangle of A is stored.
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
        /// or the number of subdiagonals if UPLO = &#39;L&#39;.  KA &gt;= KB &gt;= 0.
        /// </param>
        /// <param name="ab">
        /// [in,out] AB is COMPLEX*16 array, dimension (LDAB,N).
        /// On entry, the upper or lower triangle of the Hermitian band
        /// matrix A, stored in the first ka+1 rows of the array.  The
        /// j-th column of A is stored in the j-th column of the array AB
        /// as follows:
        /// if UPLO = &#39;U&#39;, AB(ka+1+i-j,j) = A(i,j) for max(1,j-ka)&lt;=i&lt;=j;
        /// if UPLO = &#39;L&#39;, AB(1+i-j,j)    = A(i,j) for j&lt;=i&lt;=min(n,j+ka).
        /// 
        /// On exit, the transformed matrix X**H*A*X, stored in the same
        /// format as A.
        /// </param>
        /// <param name="ldab">
        /// [in] LDAB is INTEGER.
        /// The leading dimension of the array AB.  LDAB &gt;= KA+1.
        /// </param>
        /// <param name="bb">
        /// [in] BB is COMPLEX*16 array, dimension (LDBB,N).
        /// The banded factor S from the split Cholesky factorization of
        /// B, as returned by ZPBSTF, stored in the first kb+1 rows of
        /// the array.
        /// </param>
        /// <param name="ldbb">
        /// [in] LDBB is INTEGER.
        /// The leading dimension of the array BB.  LDBB &gt;= KB+1.
        /// </param>
        /// <param name="x">
        /// [out] X is COMPLEX*16 array, dimension (LDX,N).
        /// If VECT = &#39;V&#39;, the n-by-n matrix X.
        /// If VECT = &#39;N&#39;, the array X is not referenced.
        /// </param>
        /// <param name="ldx">
        /// [in] LDX is INTEGER.
        /// The leading dimension of the array X.
        /// LDX &gt;= max(1,N) if VECT = &#39;V&#39;; LDX &gt;= 1 otherwise.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zhbgst", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zhbgst(
            MatrixLayout matrixLayout,
            char vect,
            char uplo,
            int n,
            int ka,
            int kb,
            Complex* ab,
            int ldab,
            Complex* bb,
            int ldbb,
            Complex* x,
            int ldx);
    }
}
