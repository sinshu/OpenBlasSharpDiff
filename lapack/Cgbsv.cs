using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CGBSV computes the solution to a complex system of linear equations
        /// A * X = B, where A is a band matrix of order N with KL subdiagonals
        /// and KU superdiagonals, and X and B are N-by-NRHS matrices.
        /// </para>
        /// <para>
        /// The LU decomposition with partial pivoting and row interchanges is
        /// used to factor A as A = L * U, where L is a product of permutation
        /// and unit lower triangular matrices with KL subdiagonals, and U is
        /// upper triangular with KL+KU superdiagonals.  The factored form of A
        /// is then used to solve the system of equations A * X = B.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of linear equations, i.e., the order of the
        /// matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="kl">
        /// [in] KL is INTEGER.
        /// The number of subdiagonals within the band of A.  KL &gt;= 0.
        /// </param>
        /// <param name="ku">
        /// [in] KU is INTEGER.
        /// The number of superdiagonals within the band of A.  KU &gt;= 0.
        /// </param>
        /// <param name="nrhs">
        /// [in] NRHS is INTEGER.
        /// The number of right hand sides, i.e., the number of columns
        /// of the matrix B.  NRHS &gt;= 0.
        /// </param>
        /// <param name="ab">
        /// [in,out] AB is COMPLEX array, dimension (LDAB,N).
        /// On entry, the matrix A in band storage, in rows KL+1 to
        /// 2*KL+KU+1; rows 1 to KL of the array need not be set.
        /// The j-th column of A is stored in the j-th column of the
        /// array AB as follows:
        /// AB(KL+KU+1+i-j,j) = A(i,j) for max(1,j-KU)&lt;=i&lt;=min(N,j+KL)
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
        /// [out] IPIV is INTEGER array, dimension (N).
        /// The pivot indices that define the permutation matrix P;
        /// row i of the matrix was interchanged with row IPIV(i).
        /// </param>
        /// <param name="b">
        /// [in,out] B is COMPLEX array, dimension (LDB,NRHS).
        /// On entry, the N-by-NRHS right hand side matrix B.
        /// On exit, if INFO = 0, the N-by-NRHS solution matrix X.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B.  LDB &gt;= max(1,N).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0:  if INFO = i, U(i,i) is exactly zero.  The factorization
        /// has been completed, but the factor U is exactly
        /// singular, and the solution has not been computed.
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
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_cgbsv", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Cgbsv(
            MatrixLayout matrixLayout,
            int n,
            int kl,
            int ku,
            int nrhs,
            Complex32* ab,
            int ldab,
            int* ipiv,
            Complex32* b,
            int ldb);
    }
}
