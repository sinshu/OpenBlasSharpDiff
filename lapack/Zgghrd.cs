﻿using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZGGHRD reduces a pair of complex matrices (A,B) to generalized upper
        /// Hessenberg form using unitary transformations, where A is a
        /// general matrix and B is upper triangular.  The form of the
        /// generalized eigenvalue problem is
        ///    A*x = lambda*B*x,
        /// and B is typically made upper triangular by computing its QR
        /// factorization and moving the unitary matrix Q to the left side
        /// of the equation.
        /// </para>
        /// <para>
        /// This subroutine simultaneously reduces A to a Hessenberg matrix H:
        ///    Q**H*A*Z = H
        /// and transforms B to another upper triangular matrix T:
        ///    Q**H*B*Z = T
        /// in order to reduce the problem to its standard form
        ///    H*y = lambda*T*y
        /// where y = Z**H*x.
        /// </para>
        /// <para>
        /// The unitary matrices Q and Z are determined as products of Givens
        /// rotations.  They may either be formed explicitly, or they may be
        /// postmultiplied into input matrices Q1 and Z1, so that
        ///      Q1 * A * Z1**H = (Q1*Q) * H * (Z1*Z)**H
        ///      Q1 * B * Z1**H = (Q1*Q) * T * (Z1*Z)**H
        /// If Q1 is the unitary matrix from the QR factorization of B in the
        /// original equation A*x = lambda*B*x, then ZGGHRD reduces the original
        /// problem to generalized Hessenberg form.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="compq">
        /// [in] COMPQ is CHARACTER*1.
        /// = &#39;N&#39;: do not compute Q;
        /// = &#39;I&#39;: Q is initialized to the unit matrix, and the
        /// unitary matrix Q is returned;
        /// = &#39;V&#39;: Q must contain a unitary matrix Q1 on entry,
        /// and the product Q1*Q is returned.
        /// </param>
        /// <param name="compz">
        /// [in] COMPZ is CHARACTER*1.
        /// = &#39;N&#39;: do not compute Z;
        /// = &#39;I&#39;: Z is initialized to the unit matrix, and the
        /// unitary matrix Z is returned;
        /// = &#39;V&#39;: Z must contain a unitary matrix Z1 on entry,
        /// and the product Z1*Z is returned.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrices A and B.  N &gt;= 0.
        /// </param>
        /// <param name="ilo">
        /// [in] ILO is INTEGER.
        /// </param>
        /// <param name="ihi">
        /// [in] IHI is INTEGER.
        /// 
        /// ILO and IHI mark the rows and columns of A which are to be
        /// reduced.  It is assumed that A is already upper triangular
        /// in rows and columns 1:ILO-1 and IHI+1:N.  ILO and IHI are
        /// normally set by a previous call to ZGGBAL; otherwise they
        /// should be set to 1 and N respectively.
        /// 1 &lt;= ILO &lt;= IHI &lt;= N, if N &gt; 0; ILO=1 and IHI=0, if N=0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is COMPLEX*16 array, dimension (LDA, N).
        /// On entry, the N-by-N general matrix to be reduced.
        /// On exit, the upper triangle and the first subdiagonal of A
        /// are overwritten with the upper Hessenberg matrix H, and the
        /// rest is set to zero.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="b">
        /// [in,out] B is COMPLEX*16 array, dimension (LDB, N).
        /// On entry, the N-by-N upper triangular matrix B.
        /// On exit, the upper triangular matrix T = Q**H B Z.  The
        /// elements below the diagonal are set to zero.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B.  LDB &gt;= max(1,N).
        /// </param>
        /// <param name="q">
        /// [in,out] Q is COMPLEX*16 array, dimension (LDQ, N).
        /// On entry, if COMPQ = &#39;V&#39;, the unitary matrix Q1, typically
        /// from the QR factorization of B.
        /// On exit, if COMPQ=&#39;I&#39;, the unitary matrix Q, and if
        /// COMPQ = &#39;V&#39;, the product Q1*Q.
        /// Not referenced if COMPQ=&#39;N&#39;.
        /// </param>
        /// <param name="ldq">
        /// [in] LDQ is INTEGER.
        /// The leading dimension of the array Q.
        /// LDQ &gt;= N if COMPQ=&#39;V&#39; or &#39;I&#39;; LDQ &gt;= 1 otherwise.
        /// </param>
        /// <param name="z">
        /// [in,out] Z is COMPLEX*16 array, dimension (LDZ, N).
        /// On entry, if COMPZ = &#39;V&#39;, the unitary matrix Z1.
        /// On exit, if COMPZ=&#39;I&#39;, the unitary matrix Z, and if
        /// COMPZ = &#39;V&#39;, the product Z1*Z.
        /// Not referenced if COMPZ=&#39;N&#39;.
        /// </param>
        /// <param name="ldz">
        /// [in] LDZ is INTEGER.
        /// The leading dimension of the array Z.
        /// LDZ &gt;= N if COMPZ=&#39;V&#39; or &#39;I&#39;; LDZ &gt;= 1 otherwise.
        /// </param>
        /// <returns>
        /// = 0:  successful exit.
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value.
        /// </returns>
        /// <remarks>
        /// <para>
        ///  This routine reduces A to Hessenberg and B to triangular form by
        ///  an unblocked reduction, as described in _Matrix_Computations_,
        ///  by Golub and van Loan (Johns Hopkins Press).
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zgghrd", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zgghrd(
            MatrixLayout matrixLayout,
            char compq,
            char compz,
            int n,
            int ilo,
            int ihi,
            Complex* a,
            int lda,
            Complex* b,
            int ldb,
            Complex* q,
            int ldq,
            Complex* z,
            int ldz);
    }
}
