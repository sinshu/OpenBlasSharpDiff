using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// Level 3 BLAS like routine for A in RFP Format.
        /// </para>
        /// <para>
        /// DTFSM  solves the matrix equation
        /// </para>
        /// <para>
        ///    op( A )*X = alpha*B  or  X*op( A ) = alpha*B
        /// </para>
        /// <para>
        /// where alpha is a scalar, X and B are m by n matrices, A is a unit, or
        /// non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
        /// </para>
        /// <para>
        ///    op( A ) = A   or   op( A ) = A**T.
        /// </para>
        /// <para>
        /// A is in Rectangular Full Packed (RFP) Format.
        /// </para>
        /// <para>
        /// The matrix X is overwritten on B.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="transr">
        /// [in] TRANSR is CHARACTER*1.
        /// = &#39;N&#39;:  The Normal Form of RFP A is stored;
        /// = &#39;T&#39;:  The Transpose Form of RFP A is stored.
        /// </param>
        /// <param name="side">
        /// [in] SIDE is CHARACTER*1.
        /// On entry, SIDE specifies whether op( A ) appears on the left
        /// or right of X as follows:
        /// 
        /// SIDE = &#39;L&#39; or &#39;l&#39;   op( A )*X = alpha*B.
        /// 
        /// SIDE = &#39;R&#39; or &#39;r&#39;   X*op( A ) = alpha*B.
        /// 
        /// Unchanged on exit.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// On entry, UPLO specifies whether the RFP matrix A came from
        /// an upper or lower triangular matrix as follows:
        /// UPLO = &#39;U&#39; or &#39;u&#39; RFP A came from an upper triangular matrix
        /// UPLO = &#39;L&#39; or &#39;l&#39; RFP A came from a  lower triangular matrix
        /// 
        /// Unchanged on exit.
        /// </param>
        /// <param name="trans">
        /// [in] TRANS is CHARACTER*1.
        /// On entry, TRANS  specifies the form of op( A ) to be used
        /// in the matrix multiplication as follows:
        /// 
        /// TRANS  = &#39;N&#39; or &#39;n&#39;   op( A ) = A.
        /// 
        /// TRANS  = &#39;T&#39; or &#39;t&#39;   op( A ) = A&#39;.
        /// 
        /// Unchanged on exit.
        /// </param>
        /// <param name="diag">
        /// [in] DIAG is CHARACTER*1.
        /// On entry, DIAG specifies whether or not RFP A is unit
        /// triangular as follows:
        /// 
        /// DIAG = &#39;U&#39; or &#39;u&#39;   A is assumed to be unit triangular.
        /// 
        /// DIAG = &#39;N&#39; or &#39;n&#39;   A is not assumed to be unit
        /// triangular.
        /// 
        /// Unchanged on exit.
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// On entry, M specifies the number of rows of B. M must be at
        /// least zero.
        /// Unchanged on exit.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// On entry, N specifies the number of columns of B.  N must be
        /// at least zero.
        /// Unchanged on exit.
        /// </param>
        /// <param name="alpha">
        /// [in] ALPHA is DOUBLE PRECISION.
        /// On entry,  ALPHA specifies the scalar  alpha. When  alpha is
        /// zero then  A is not referenced and  B need not be set before
        /// entry.
        /// Unchanged on exit.
        /// </param>
        /// <param name="a">
        /// [in] A is DOUBLE PRECISION array, dimension (NT).
        /// NT = N*(N+1)/2. On entry, the matrix A in RFP Format.
        /// RFP Format is described by TRANSR, UPLO and N as follows:
        /// If TRANSR=&#39;N&#39; then RFP A is (0:N,0:K-1) when N is even;
        /// K=N/2. RFP A is (0:N-1,0:K) when N is odd; K=N/2. If
        /// TRANSR = &#39;T&#39; then RFP is the transpose of RFP A as
        /// defined when TRANSR = &#39;N&#39;. The contents of RFP A are defined
        /// by UPLO as follows: If UPLO = &#39;U&#39; the RFP A contains the NT
        /// elements of upper packed A either in normal or
        /// transpose Format. If UPLO = &#39;L&#39; the RFP A contains
        /// the NT elements of lower packed A either in normal or
        /// transpose Format. The LDA of RFP A is (N+1)/2 when
        /// TRANSR = &#39;T&#39;. When TRANSR is &#39;N&#39; the LDA is N+1 when N is
        /// even and is N when is odd.
        /// See the Note below for more details. Unchanged on exit.
        /// </param>
        /// <param name="b">
        /// [in,out] B is DOUBLE PRECISION array, dimension (LDB,N).
        /// Before entry,  the leading  m by n part of the array  B must
        /// contain  the  right-hand  side  matrix  B,  and  on exit  is
        /// overwritten by the solution matrix  X.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// On entry, LDB specifies the first dimension of B as declared
        /// in  the  calling  (sub)  program.   LDB  must  be  at  least
        /// max( 1, m ).
        /// Unchanged on exit.
        /// </param>
        /// <remarks>
        /// <para>
        ///  We first consider Rectangular Full Packed (RFP) Format when N is
        ///  even. We give an example where N = 6.
        /// </para>
        /// <para>
        ///      AP is Upper             AP is Lower
        /// </para>
        /// <para>
        ///   00 01 02 03 04 05       00
        ///      11 12 13 14 15       10 11
        ///         22 23 24 25       20 21 22
        ///            33 34 35       30 31 32 33
        ///               44 45       40 41 42 43 44
        ///                  55       50 51 52 53 54 55
        /// </para>
        /// <para>
        ///  Let TRANSR = &#39;N&#39;. RFP holds AP as follows:
        ///  For UPLO = &#39;U&#39; the upper trapezoid A(0:5,0:2) consists of the last
        ///  three columns of AP upper. The lower triangle A(4:6,0:2) consists of
        ///  the transpose of the first three columns of AP upper.
        ///  For UPLO = &#39;L&#39; the lower trapezoid A(1:6,0:2) consists of the first
        ///  three columns of AP lower. The upper triangle A(0:2,0:2) consists of
        ///  the transpose of the last three columns of AP lower.
        ///  This covers the case N even and TRANSR = &#39;N&#39;.
        /// </para>
        /// <para>
        ///         RFP A                   RFP A
        /// </para>
        /// <para>
        ///        03 04 05                33 43 53
        ///        13 14 15                00 44 54
        ///        23 24 25                10 11 55
        ///        33 34 35                20 21 22
        ///        00 44 45                30 31 32
        ///        01 11 55                40 41 42
        ///        02 12 22                50 51 52
        /// </para>
        /// <para>
        ///  Now let TRANSR = &#39;T&#39;. RFP A in both UPLO cases is just the
        ///  transpose of RFP A above. One therefore gets:
        /// </para>
        /// <para>
        ///           RFP A                   RFP A
        /// </para>
        /// <para>
        ///     03 13 23 33 00 01 02    33 00 10 20 30 40 50
        ///     04 14 24 34 44 11 12    43 44 11 21 31 41 51
        ///     05 15 25 35 45 55 22    53 54 55 22 32 42 52
        /// </para>
        /// <para>
        ///  We then consider Rectangular Full Packed (RFP) Format when N is
        ///  odd. We give an example where N = 5.
        /// </para>
        /// <para>
        ///     AP is Upper                 AP is Lower
        /// </para>
        /// <para>
        ///   00 01 02 03 04              00
        ///      11 12 13 14              10 11
        ///         22 23 24              20 21 22
        ///            33 34              30 31 32 33
        ///               44              40 41 42 43 44
        /// </para>
        /// <para>
        ///  Let TRANSR = &#39;N&#39;. RFP holds AP as follows:
        ///  For UPLO = &#39;U&#39; the upper trapezoid A(0:4,0:2) consists of the last
        ///  three columns of AP upper. The lower triangle A(3:4,0:1) consists of
        ///  the transpose of the first two columns of AP upper.
        ///  For UPLO = &#39;L&#39; the lower trapezoid A(0:4,0:2) consists of the first
        ///  three columns of AP lower. The upper triangle A(0:1,1:2) consists of
        ///  the transpose of the last two columns of AP lower.
        ///  This covers the case N odd and TRANSR = &#39;N&#39;.
        /// </para>
        /// <para>
        ///         RFP A                   RFP A
        /// </para>
        /// <para>
        ///        02 03 04                00 33 43
        ///        12 13 14                10 11 44
        ///        22 23 24                20 21 22
        ///        00 33 34                30 31 32
        ///        01 11 44                40 41 42
        /// </para>
        /// <para>
        ///  Now let TRANSR = &#39;T&#39;. RFP A in both UPLO cases is just the
        ///  transpose of RFP A above. One therefore gets:
        /// </para>
        /// <para>
        ///           RFP A                   RFP A
        /// </para>
        /// <para>
        ///     02 12 22 00 01             00 10 20 30 40 50
        ///     03 13 23 33 11             33 11 21 31 41 51
        ///     04 14 24 34 44             43 44 22 32 42 52
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dtfsm", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dtfsm(
            MatrixLayout matrixLayout,
            char transr,
            char side,
            char uplo,
            char trans,
            char diag,
            int m,
            int n,
            double alpha,
            double* a,
            double* b,
            int ldb);
    }
}
