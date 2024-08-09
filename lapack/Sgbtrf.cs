using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// SGBTRF computes an LU factorization of a real m-by-n band matrix A
        /// using partial pivoting with row interchanges.
        /// </para>
        /// <para>
        /// This is the blocked version of the algorithm, calling Level 3 BLAS.
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
        /// <param name="kl">
        /// [in] KL is INTEGER.
        /// The number of subdiagonals within the band of A.  KL &gt;= 0.
        /// </param>
        /// <param name="ku">
        /// [in] KU is INTEGER.
        /// The number of superdiagonals within the band of A.  KU &gt;= 0.
        /// </param>
        /// <param name="ab">
        /// [in,out] AB is REAL array, dimension (LDAB,N).
        /// On entry, the matrix A in band storage, in rows KL+1 to
        /// 2*KL+KU+1; rows 1 to KL of the array need not be set.
        /// The j-th column of A is stored in the j-th column of the
        /// array AB as follows:
        /// AB(kl+ku+1+i-j,j) = A(i,j) for max(1,j-ku)&lt;=i&lt;=min(m,j+kl)
        /// 
        /// On exit, details of the factorization: U is stored as an
        /// upper triangular band matrix with KL+KU superdiagonals in
        /// rows 1 to KL+KU+1, and the multipliers used during the
        /// factorization are stored in rows KL+KU+2 to 2*KL+KU+1.
        /// See below for further details.
        /// </param>
        /// <param name="ldab">
        /// [in] LDAB is INTEGER.
        /// The leading dimension of the array AB.  LDAB &gt;= 2*KL+KU+1.
        /// </param>
        /// <param name="ipiv">
        /// [out] IPIV is INTEGER array, dimension (min(M,N)).
        /// The pivot indices; for 1 &lt;= i &lt;= min(M,N), row i of the
        /// matrix was interchanged with row IPIV(i).
        /// </param>
        /// <returns>
        /// = 0: successful exit
        /// &lt; 0: if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0: if INFO = +i, U(i,i) is exactly zero. The factorization
        /// has been completed, but the factor U is exactly
        /// singular, and division by zero will occur if it is used
        /// to solve a system of equations.
        /// </returns>
        /// <remarks>
        /// <para>
        ///  The band storage scheme is illustrated by the following example, when
        ///  M = N = 6, KL = 2, KU = 1:
        /// </para>
        /// <para>
        ///  On entry:                       On exit:
        /// </para>
        /// <para>
        ///      *    *    *    +    +    +       *    *    *   u14  u25  u36
        ///      *    *    +    +    +    +       *    *   u13  u24  u35  u46
        ///      *   a12  a23  a34  a45  a56      *   u12  u23  u34  u45  u56
        ///     a11  a22  a33  a44  a55  a66     u11  u22  u33  u44  u55  u66
        ///     a21  a32  a43  a54  a65   *      m21  m32  m43  m54  m65   *
        ///     a31  a42  a53  a64   *    *      m31  m42  m53  m64   *    *
        /// </para>
        /// <para>
        ///  Array elements marked * are not used by the routine; elements marked
        ///  + need not be set on entry, but are required by the routine to store
        ///  elements of U because of fill-in resulting from the row interchanges.
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_sgbtrf", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Sgbtrf(
            MatrixLayout matrixLayout,
            int m,
            int n,
            int kl,
            int ku,
            float* ab,
            int ldab,
            int* ipiv);
    }
}
