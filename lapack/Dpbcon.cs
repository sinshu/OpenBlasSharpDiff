using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DPBCON estimates the reciprocal of the condition number (in the
        /// 1-norm) of a real symmetric positive definite band matrix using the
        /// Cholesky factorization A = U**T*U or A = L*L**T computed by DPBTRF.
        /// </para>
        /// <para>
        /// An estimate is obtained for norm(inv(A)), and the reciprocal of the
        /// condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// = &#39;U&#39;:  Upper triangular factor stored in AB;
        /// = &#39;L&#39;:  Lower triangular factor stored in AB.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="kd">
        /// [in] KD is INTEGER.
        /// The number of superdiagonals of the matrix A if UPLO = &#39;U&#39;,
        /// or the number of subdiagonals if UPLO = &#39;L&#39;.  KD &gt;= 0.
        /// </param>
        /// <param name="ab">
        /// [in] AB is DOUBLE PRECISION array, dimension (LDAB,N).
        /// The triangular factor U or L from the Cholesky factorization
        /// A = U**T*U or A = L*L**T of the band matrix A, stored in the
        /// first KD+1 rows of the array.  The j-th column of U or L is
        /// stored in the j-th column of the array AB as follows:
        /// if UPLO =&#39;U&#39;, AB(kd+1+i-j,j) = U(i,j) for max(1,j-kd)&lt;=i&lt;=j;
        /// if UPLO =&#39;L&#39;, AB(1+i-j,j)    = L(i,j) for j&lt;=i&lt;=min(n,j+kd).
        /// </param>
        /// <param name="ldab">
        /// [in] LDAB is INTEGER.
        /// The leading dimension of the array AB.  LDAB &gt;= KD+1.
        /// </param>
        /// <param name="anorm">
        /// [in] ANORM is DOUBLE PRECISION.
        /// The 1-norm (or infinity-norm) of the symmetric band matrix A.
        /// </param>
        /// <param name="rcond">
        /// [out] RCOND is DOUBLE PRECISION.
        /// The reciprocal of the condition number of the matrix A,
        /// computed as RCOND = 1/(ANORM * AINVNM), where AINVNM is an
        /// estimate of the 1-norm of inv(A) computed in this routine.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dpbcon", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dpbcon(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            int kd,
            double* ab,
            int ldab,
            double anorm,
            double* rcond);
    }
}
