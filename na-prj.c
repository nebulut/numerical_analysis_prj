#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>

// Yapı tanımı
typedef struct {
    double *x;
    double *y;
    int n;
} VeriSeti;

// Fonksiyon prototipleri
void bosluklariTemizle(char *dest, const char *src);
double ifadeyiDegerlendir(char **expr, double x);
double terimDegerlendir(char **expr, double x);
double faktorDegerlendir(char **expr, double x);
double atomDegerlendir(char **expr, double x);
double temelLogHesapla(double taban, double deger);
double denklemDegerlendir(char *ifade, double deger);
void bisectionYontemi();
void regulaFalsiYontemi();
void newtonRaphsonYontemi();
void matrisTersiniBulmaIslemi();
void gaussEliminasyonIslemi();
void gaussSeidelIslemi();
void sayisalTurevIslemi();
void simpsonYontemi();
void trapezYontemi();
void interpolasyonIslemi();
void menu();
double ileriFark(char *ifade, double deger, double h);
double geriFark(char *ifade, double deger, double h);
double merkeziFark(char *ifade, double deger, double h);
double sayisalTurev(char *ifade, double xDegeri, double h, int turevTuru);
double gregoryNewtonInterpolasyon(VeriSeti veri, double *ileri_fark, double x);
void gaussEliminasyon(double **matris, double *sonuc, double *cozum, int n);
double** matrisOlustur(int n);
void matrisSerbestBirak(double **matris, int n);
void matrisVeSonucAl(double **matris, double *sonuc, int n);
void matrisAl(double **matris, int n);
void matrisYazdir(double **matris, int n);
void matrisTersi(double **matris, double **ters, int n);
void vektorYazdir(double *vektor, int n);
void gaussSeidel(double **matris, double *sonuc, double *cozum, int n, int iterasyon, double tol);
void veriSetiOlustur(VeriSeti *veri, int n);
void veriSetiSerbestBirak(VeriSeti *veri);
void ileriFarkHesapla(double *ileri_fark, VeriSeti veri);

int main() {
    menu();
    return 0;
}

// Fonksiyon tanımlamaları

double atomDegerlendir(char **expr, double x) {
    double deger = 0;
    while (**expr == ' ') (*expr)++;  // Boşlukları atla

    if (**expr == '(') {  // Parantez içi ifadeyi değerlendir
        (*expr)++;
        deger = ifadeyiDegerlendir(expr, x);
        if (**expr == ')') (*expr)++;
    } else if (isdigit(**expr) || **expr == '.') {  // Sayısal değerleri işle
        deger = strtod(*expr, expr);
    } else if (strncmp(*expr, "asin", 4) == 0) {
        *expr += 4; deger = asin(ifadeyiDegerlendir(expr, x));
    } else if (strncmp(*expr, "acos", 4) == 0) {
        *expr += 4; deger = acos(ifadeyiDegerlendir(expr, x));
    } else if (strncmp(*expr, "atan", 4) == 0) {
        *expr += 4; deger = atan(ifadeyiDegerlendir(expr, x));
    } else if (strncmp(*expr, "sin", 3) == 0) {
        *expr += 3; deger = sin(ifadeyiDegerlendir(expr, x) * M_PI / 180.0);
    } else if (strncmp(*expr, "cos", 3) == 0) {
        *expr += 3; deger = cos(ifadeyiDegerlendir(expr, x) * M_PI / 180.0);
    } else if (strncmp(*expr, "tan", 3) == 0) {
        *expr += 3; deger = tan(ifadeyiDegerlendir(expr, x) * M_PI / 180.0);
    } else if (strncmp(*expr, "cot", 3) == 0) {
        *expr += 3; deger = 1 / tan(ifadeyiDegerlendir(expr, x) * M_PI / 180.0);
    } else if (strncmp(*expr, "sec", 3) == 0) {
        *expr += 3; deger = 1 / cos(ifadeyiDegerlendir(expr, x) * M_PI / 180.0);
    } else if (strncmp(*expr, "csc", 3) == 0) {
        *expr += 3; deger = 1 / sin(ifadeyiDegerlendir(expr, x) * M_PI / 180.0);
    } else if (strncmp(*expr, "log_", 4) == 0) {
        *expr += 4;
        double base = ifadeyiDegerlendir(expr, x);
        deger = temelLogHesapla(base, ifadeyiDegerlendir(expr, x));
    } else if (strncmp(*expr, "ln", 2) == 0) {
        *expr += 2; deger = log(ifadeyiDegerlendir(expr, x));
    } else if (**expr == 'e') {
        (*expr)++; deger = M_E;
    } else if (**expr == 'x' || **expr == 'X') {
        (*expr)++; deger = x;  // x değişkenini değerlendir
    } else {
        printf("Beklenmeyen karakter: %c\n", **expr);
        exit(1);
    }
    return deger;
}

// Faktörleri işleyen fonksiyon (Üs alma)
double faktorDegerlendir(char **expr, double x) {
    double deger = atomDegerlendir(expr, x);
    while (**expr == '^') {  // Üs işlemi
        (*expr)++;
        deger = pow(deger, atomDegerlendir(expr, x));
    }
    return deger;
}

// Terimleri işleyen fonksiyon (Çarpma ve bölme)
double terimDegerlendir(char **expr, double x) {
    double deger = faktorDegerlendir(expr, x);
    while (**expr == '*' || **expr == '/') {
        char op = **expr;
        (*expr)++;
        double sonrakiDeger = faktorDegerlendir(expr, x);
        if (op == '*') deger *= sonrakiDeger;
        else deger /= sonrakiDeger;
    }
    return deger;
}

// İfadeleri değerlendiren ana fonksiyon (Toplama ve çıkarma)
double ifadeyiDegerlendir(char **expr, double x) {
    double deger = terimDegerlendir(expr, x);
    while (**expr == '+' || **expr == '-') {
        char op = **expr;
        (*expr)++;
        double sonrakiDeger = terimDegerlendir(expr, x);
        if (op == '+') deger += sonrakiDeger;
        else deger -= sonrakiDeger;
    }
    return deger;
}

// Kullanıcıdan gelen ifadeyi değerlendiren fonksiyon
double denklemDegerlendir(char *ifade, double deger) {
    char *ptr = ifade;
    char temizIfade[1024];
    bosluklariTemizle(temizIfade, ifade);
    ptr = temizIfade;
    return ifadeyiDegerlendir(&ptr, deger);
}

// Boşlukları temizleyen yardımcı fonksiyon
void bosluklariTemizle(char *dest, const char *src) {
    while (*src) {
        if (*src != ' ') {
            *dest++ = *src;
        }
        src++;
    }
    *dest = '\0';
}

// Temel logaritma hesaplama fonksiyonu
double temelLogHesapla(double taban, double deger) {
    return log(deger) / log(taban);
}

// Bisection yöntemi uygulayan fonksiyon
void bisectionYontemi() {
    char ifade[1024];
    double a, b, tolerans, fa, fb, c, fc;
    int iterasyon;

    printf("Denklemi giriniz (x değişkenini kullanarak, örn: 'x^2 - 4*x + 4'): ");
    fgets(ifade, sizeof(ifade), stdin);
    ifade[strcspn(ifade, "\n")] = 0;  // Newline karakterini temizle

    printf("Aralığın başlangıç değeri a: ");
    scanf("%lf", &a);
    while((getchar()) != '\n');
    printf("Aralığın bitiş değeri b: ");
    scanf("%lf", &b);
    while((getchar()) != '\n');
    printf("Tolerans (epsilon) değeri: ");
    scanf("%lf", &tolerans);
    while((getchar()) != '\n');
    printf("Maksimum iterasyon sayısı: ");
    scanf("%d", &iterasyon);
    while((getchar()) != '\n');

    fa = denklemDegerlendir(ifade, a);
    fb = denklemDegerlendir(ifade, b);

    printf("fa: %f fb: %f", fa, fb);

    if (fa * fb > 0) {
        printf("Verilen aralıkta kök yok veya kök tekil olmayabilir.\n");
        return;
    }

    int i;
    for (i = 0; i < iterasyon; i++) {
        c = (a + b) / 2.0;
        fc = denklemDegerlendir(ifade, c);

        if (fc == 0 || (b - a) / 2.0 < tolerans) {
            printf("Kök bulundu: x = %.12f\n", c);
            return;
        }

        if (fa * fc <= 0) {
            b = c;
            fb = fc;
        } else {
            a = c;
            fa = fc;
        }

        printf("Iterasyon %d: c = %.12f, f(c) = %.12f\n", i+1, c, fc);
    }

    printf("Kök yaklaşık olarak: x = %.12f\n", (a + b) / 2.0);
}

// Regula-Falsi yöntemi uygulayan fonksiyon
void regulaFalsiYontemi() {
    char ifade[1024];
    double a, b, tolerans, fa, fb, c, fc, hata;
    int iterasyon;

    printf("Denklemi giriniz (x değişkenini kullanarak, örn: 'x^2 - 4*x + 4'): ");
    fgets(ifade, sizeof(ifade), stdin);
    ifade[strcspn(ifade, "\n")] = 0;  // Newline karakterini temizle

    printf("Aralığın başlangıç değeri a: ");
    scanf("%lf", &a);
    while((getchar()) != '\n');
    printf("Aralığın bitiş değeri b: ");
    scanf("%lf", &b);
    while((getchar()) != '\n');
    printf("Tolerans (epsilon) değeri: ");
    scanf("%lf", &tolerans);
    while((getchar()) != '\n');
    printf("Maksimum iterasyon sayısı: ");
    scanf("%d", &iterasyon);
    while((getchar()) != '\n');

    fa = denklemDegerlendir(ifade, a);
    fb = denklemDegerlendir(ifade, b);

    if (fa * fb > 0) {
        printf("Verilen aralıkta kök yok veya kök tekil olmayabilir.\n");
        return;
    }

    int i;
    for (i = 0; i < iterasyon; i++) {
        c = a - fa * (b - a) / (fb - fa);  // Kök tahmini
        fc = denklemDegerlendir(ifade, c);

        if (fc == 0 || fabs(fc) < tolerans) {
            printf("Kök bulundu: x = %.12f\n", c);
            return;
        }

        // Kökün konumunu güncelle
        if (fa * fc <= 0) {
            b = c;
            fb = fc;
        } else {
            a = c;
            fa = fc;
        }
        hata = fabs(b - a);  // Hata hesaplama

        printf("Iterasyon %d: c = %.12f, f(c) = %.12f\n", i+1, c, fc);
    }

    printf("Kök yaklaşık olarak: x = %.12f\n", c);
}

// Sayısal türev hesaplama fonksiyonları
double ileriFark(char *ifade, double deger, double h) {
    return (denklemDegerlendir(ifade, deger + h) - denklemDegerlendir(ifade, deger)) / h;
}

double geriFark(char *ifade, double deger, double h) {
    return (denklemDegerlendir(ifade, deger) - denklemDegerlendir(ifade, deger - h)) / h;
}

double merkeziFark(char *ifade, double deger, double h) {
    return (denklemDegerlendir(ifade, deger + h) - denklemDegerlendir(ifade, deger - h)) / (2 * h);
}

// Türev türü seçimine göre sayısal türevi hesaplayan fonksiyon
double sayisalTurev(char *ifade, double xDegeri, double h, int turevTuru) {
    switch (turevTuru) {
        case 1:
            return ileriFark(ifade, xDegeri, h);
        case 2:
            return geriFark(ifade, xDegeri, h);
        case 3:
            return merkeziFark(ifade, xDegeri, h);
        default:
            fprintf(stderr, "Geçersiz türev türü seçimi.\n");
            exit(1);
    }
}

// Newton-Raphson yöntemi uygulayan fonksiyon
void newtonRaphsonYontemi() {
    char ifade[1024];
    double tahminiDeger, tolerans, h = 1e-6; // h, türev için kullanılan küçük değer
    int turevTuru, iterasyon;

    printf("Denklemi giriniz (x değişkenini kullanarak, örn: 'x^2 - 4*x + 4'): ");
    fgets(ifade, sizeof(ifade), stdin);
    ifade[strcspn(ifade, "\n")] = 0;  // Newline karakterini temizle

    printf("Başlangıç tahmini: ");
    scanf("%lf", &tahminiDeger);
    while((getchar()) != '\n');
    printf("Tolerans (epsilon) değeri: ");
    scanf("%lf", &tolerans);
    while((getchar()) != '\n');
    printf("Türev tipini seçiniz (1: İleri, 2: Geri, 3: Merkezi): ");
    scanf("%d", &turevTuru);
    while((getchar()) != '\n');
    printf("Maksimum iterasyon sayısı: ");
    scanf("%d", &iterasyon);
    while((getchar()) != '\n');

    double hata;
    int i;
    for (i = 0; i < iterasyon; i++) {
        double fonksiyonDegeri = denklemDegerlendir(ifade, tahminiDeger);
        double turevDegeri = sayisalTurev(ifade, tahminiDeger, h, turevTuru);

        if (fabs(turevDegeri) < 1e-10) {  // Türev sıfıra çok yakınsa, dur
            printf("Türev sıfıra çok yakın, çözüm bulunamıyor.\n");
            return;
        }

        double oncekiDeger = tahminiDeger;
        tahminiDeger = tahminiDeger - fonksiyonDegeri / turevDegeri;  // Newton-Raphson formülü
        hata = fabs(tahminiDeger - oncekiDeger);

        printf("Iterasyon %d: tahmini kök = %.12f\n", i+1, tahminiDeger);

        if (hata < tolerans) {
            break;
        }
    }

    printf("Yaklaşık kök: %.12f\n", tahminiDeger);
}

// Simpson yöntemi uygulayan fonksiyon
void simpsonYontemi() {
    char ifade[1024];
    double a, b;

    printf("Denklemi giriniz (x değişkenini kullanarak, örn: 'sin(x) + x^2'): ");
    fgets(ifade, sizeof(ifade), stdin);
    ifade[strcspn(ifade, "\n")] = 0;  // Newline karakterini temizle

    printf("İntegral aralığının başlangıç değeri a: ");
    scanf("%lf", &a);
    while((getchar()) != '\n');
    printf("İntegral aralığının bitiş değeri b: ");
    scanf("%lf", &b);
    while((getchar()) != '\n');

    double h = (b - a) / 2.0;
    double fa = denklemDegerlendir(ifade, a);
    double fm = denklemDegerlendir(ifade, a + h);
    double fb = denklemDegerlendir(ifade, b);
    double sonuc = (h / 3.0) * (fa + 4.0 * fm + fb);

    printf("İntegralin yaklaşık değeri: %.12f\n", sonuc);
}

// Trapez yöntemi uygulayan fonksiyon
void trapezYontemi() {
    char ifade[1024];
    double a, b;

    printf("Denklemi giriniz (x değişkenini kullanarak, örn: 'sin(x) + x^2'): ");
    fgets(ifade, sizeof(ifade), stdin);
    ifade[strcspn(ifade, "\n")] = 0;  // Newline karakterini temizle

    printf("İntegral aralığının başlangıç değeri a: ");
    scanf("%lf", &a);
    while((getchar()) != '\n');
    printf("İntegral aralığının bitiş değeri b: ");
    scanf("%lf", &b);
    while((getchar()) != '\n');

    double fa = denklemDegerlendir(ifade, a);
    double fb = denklemDegerlendir(ifade, b);
    double h = b - a;
    double sonuc = (h / 2.0) * (fa + fb);

    printf("İntegralin yaklaşık değeri: %.12f\n", sonuc);
}

// Veri setini oluşturur
void veriSetiOlustur(VeriSeti *veri, int n) {
    veri->x = (double *)malloc(n * sizeof(double));
    veri->y = (double *)malloc(n * sizeof(double));
    veri->n = n;
}

// Veri setini serbest bırakır
void veriSetiSerbestBirak(VeriSeti *veri) {
    free(veri->x);
    free(veri->y);
}

// İleri fark tablosunu hesaplar
void ileriFarkHesapla(double *ileri_fark, VeriSeti veri) {
    int i, j;
    int n = veri.n;
    for (i = 0; i < n; i++) {
        ileri_fark[i] = veri.y[i];
    }
    for (i = 1; i < n; i++) {
        for (j = n - 1; j >= i; j--) {
            ileri_fark[j] = (ileri_fark[j] - ileri_fark[j - 1]);
        }
    }
}

// Gregory-Newton interpolasyonunu hesaplar
double gregoryNewtonInterpolasyon(VeriSeti veri, double *ileri_fark, double x) {
    double sonuc = ileri_fark[0];
    double carpim_terimi = 1.0;
    double h = veri.x[1] - veri.x[0];
    double p = (x - veri.x[0]) / h;
    int i;

    for (i = 1; i < veri.n; i++) {
        carpim_terimi *= (p - (i - 1)) / i;
        sonuc += carpim_terimi * ileri_fark[i];
    }

    return sonuc;
}

// Gregory-Newton interpolasyonu işlemi
void interpolasyonIslemi() {
    int n;
    double x;

    // Kullanıcıdan veri sayısını al
    printf("Veri noktalarının sayısını girin: ");
    scanf("%d", &n);
    while((getchar()) != '\n');

    // Veri setini oluştur
    VeriSeti veri;
    veriSetiOlustur(&veri, n);

    // Kullanıcıdan veri noktalarını al
    printf("x ve y değerlerini girin:\n");
    int i;
    for (i = 0; i < n; i++) {
        printf("x[%d]: ", i);
        scanf("%lf", &veri.x[i]);
        while((getchar()) != '\n');
        printf("y[%d]: ", i);
        scanf("%lf", &veri.y[i]);
        while((getchar()) != '\n');
    }

    // İleri fark tablosunu hesapla
    double *ileri_fark = (double *)malloc(n * sizeof(double));
    ileriFarkHesapla(ileri_fark, veri);

    // Kullanıcıdan interpolasyon yapılacak noktayı al
    printf("Interpolasyon yapılacak noktayı girin: ");
    scanf("%lf", &x);
    while((getchar()) != '\n');

    // Gregory-Newton interpolasyonunu gerçekleştir
    double sonuc = gregoryNewtonInterpolasyon(veri, ileri_fark, x);
    printf("Interpolasyon sonucu: %lf\n", sonuc);

    // Belleği serbest bırak
    free(ileri_fark);
    veriSetiSerbestBirak(&veri);
}

// Gauss eliminasyon işlemi
void gaussEliminasyonIslemi() {
    int n;

    // Kullanıcıdan matris boyutunu al
    printf("Matris boyutunu girin: ");
    scanf("%d", &n);
    while((getchar()) != '\n');

    // Matris ve sonuç vektörünü oluştur
    double **matris = matrisOlustur(n);
    double *sonuc = (double *)malloc(n * sizeof(double));
    double *cozum = (double *)malloc(n * sizeof(double));

    // Kullanıcıdan matris ve sonuç vektörünü al
    matrisVeSonucAl(matris, sonuc, n);

    // Gauss eliminasyonu ile denklemleri çöz
    gaussEliminasyon(matris, sonuc, cozum, n);

    // Çözümü ekrana yazdır
    int i;
    for (i = 0; i < n; i++) {
        printf("cozum[%d] = %lf\n", i, cozum[i]);
    }

    // Belleği serbest bırak
    matrisSerbestBirak(matris, n);
    free(sonuc);
    free(cozum);
}

// Gauss eliminasyon fonksiyonu
void gaussEliminasyon(double **matris, double *sonuc, double *cozum, int n) {
    int i, j, k;
    double oran;

    // Üst üçgen matris oluşturma
    for (i = 0; i < n; i++) {
        for (j = i + 1; j < n; j++) {
            if (matris[i][i] == 0) {
                printf("Sıfıra bölme hatası!\n");
                exit(1);
            }
            oran = matris[j][i] / matris[i][i];
            for (k = 0; k < n; k++) {
                matris[j][k] -= oran * matris[i][k];
            }
            sonuc[j] -= oran * sonuc[i];
        }
    }

    // Geri yerine koyma işlemi
    for (i = n - 1; i >= 0; i--) {
        cozum[i] = sonuc[i];
        for (j = i + 1; j < n; j++) {
            cozum[i] -= matris[i][j] * cozum[j];
        }
        cozum[i] /= matris[i][i];
    }
}

// Matris oluşturma fonksiyonu
double** matrisOlustur(int n) {
    int i;
    double **matris = (double **)malloc(n * sizeof(double *));
    for (i = 0; i < n; i++) {
        matris[i] = (double *)malloc(n * sizeof(double));
    }
    return matris;
}

// Matris serbest bırakma fonksiyonu
void matrisSerbestBirak(double **matris, int n) {
    int i;
    for (i = 0; i < n; i++) {
        free(matris[i]);
    }
    free(matris);
}

// Matris ve sonuç vektörünü kullanıcıdan alma fonksiyonu
void matrisVeSonucAl(double **matris, double *sonuc, int n) {
    printf("Matrisin elemanlarını girin:\n");
    int i, j;
    for (i = 0; i < n; i++) {
        for (j = 0; j < n; j++) {
            printf("matris[%d][%d]: ", i, j);
            scanf("%lf", &matris[i][j]);
            while((getchar()) != '\n');
        }
    }

    printf("Sonuç vektörünün elemanlarını girin:\n");
    for (i = 0; i < n; i++) {
        printf("sonuc[%d]: ", i);
        scanf("%lf", &sonuc[i]);
        while((getchar()) != '\n');
    }
}

// Kullanıcıdan NxN boyutunda matris alma fonksiyonu
void matrisAl(double **matris, int n) {
    printf("Matrisin elemanlarını girin:\n");
    int i, j;
    for (i = 0; i < n; i++) {
        for (j = 0; j < n; j++) {
            printf("matris[%d][%d]: ", i, j);
            scanf("%lf", &matris[i][j]);
            while((getchar()) != '\n');
        }
    }
}

// NxN boyutunda matris yazdırma fonksiyonu
void matrisYazdir(double **matris, int n) {
    int i, j;
    for (i = 0; i < n; i++) {
        for (j = 0; j < n; j++) {
            printf("%lf ", matris[i][j]);
        }
        printf("\n");
    }
}

// NxN boyutunda matrisin tersini hesaplama fonksiyonu
void matrisTersi(double **matris, double **ters, int n) {
    int i, j, k;
    double oran;

    // Birim matrisi ters matrise kopyala
    for (i = 0; i < n; i++) {
        for (j = 0; j < n; j++) {
            ters[i][j] = (i == j) ? 1.0 : 0.0;
        }
    }

    // Gauss-Jordan eliminasyonu
    for (i = 0; i < n; i++) {
        if (matris[i][i] == 0.0) {
            printf("Sıfıra bölme hatası!\n");
            exit(1);
        }
        for (j = 0; j < n; j++) {
            if (i != j) {
                oran = matris[j][i] / matris[i][i];
                for (k = 0; k < n; k++) {
                    matris[j][k] -= oran * matris[i][k];
                    ters[j][k] -= oran * ters[i][k];
                }
            }
        }
    }

    // Satırları normalleştir
    for (i = 0; i < n; i++) {
        oran = matris[i][i];
        for (j = 0; j < n; j++) {
            matris[i][j] /= oran;
            ters[i][j] /= oran;
        }
    }
}

// Matris tersini bulma işlemi
void matrisTersiniBulmaIslemi() {
    int n;

    // Kullanıcıdan matris boyutunu al
    printf("Matris boyutunu girin: ");
    scanf("%d", &n);
    while((getchar()) != '\n');

    // Matris ve tersini oluştur
    double **matris = matrisOlustur(n);
    double **ters = matrisOlustur(n);

    // Kullanıcıdan matris elemanlarını al
    matrisAl(matris, n);

    // Matrisin tersini hesapla
    matrisTersi(matris, ters, n);

    // Ters matrisi ekrana yazdır
    printf("Ters matris:\n");
    matrisYazdir(ters, n);

    // Belleği serbest bırak
    matrisSerbestBirak(matris, n);
    matrisSerbestBirak(ters, n);
}

// Çözüm vektörünü yazdırma fonksiyonu
void vektorYazdir(double *vektor, int n) {
    int i;
    for (i = 0; i < n; i++) {
        printf("%lf\n", vektor[i]);
    }
}

// Gauss-Seidel yöntemi işlemi
void gaussSeidelIslemi() {
    int n, iterasyon;
    double tol;

    // Kullanıcıdan matris boyutunu al
    printf("Matris boyutunu girin: ");
    scanf("%d", &n);
    while((getchar()) != '\n');

    // Matris ve sonuç vektörünü oluştur
    double **matris = matrisOlustur(n);
    double *sonuc = (double *)malloc(n * sizeof(double));
    double *cozum = (double *)malloc(n * sizeof(double));

    // Kullanıcıdan matris ve sonuç vektörünü al
    matrisVeSonucAl(matris, sonuc, n);

    // Kullanıcıdan maksimum iterasyon sayısı ve tolerans değerini al
    printf("Maksimum iterasyon sayısını girin: ");
    scanf("%d", &iterasyon);
    while((getchar()) != '\n');
    printf("Tolerans değerini girin: ");
    scanf("%lf", &tol);
    while((getchar()) != '\n');

    // Gauss-Seidel yöntemi ile denklemleri çöz
    gaussSeidel(matris, sonuc, cozum, n, iterasyon, tol);

    // Çözümü ekrana yazdır
    printf("Çözüm vektörü:\n");
    vektorYazdir(cozum, n);

    // Belleği serbest bırak
    matrisSerbestBirak(matris, n);
    free(sonuc);
    free(cozum);
}

// Gauss-Seidel yöntemi
void gaussSeidel(double **matris, double *sonuc, double *cozum, int n, int iterasyon, double tol) {
    double *onceki = (double *)malloc(n * sizeof(double));
    int i, j, iter;
    double hata;

    // Başlangıç değerini 0 olarak ata
    for (i = 0; i < n; i++) {
        cozum[i] = 0.0;
    }

    for (iter = 0; iter < iterasyon; iter++) {
        for (i = 0; i < n; i++) {
            onceki[i] = cozum[i];
        }

        for (i = 0; i < n; i++) {
            double sum = sonuc[i];
            for (j = 0; j < n; j++) {
                if (j != i) {
                    sum -= matris[i][j] * cozum[j];
                }
            }
            cozum[i] = sum / matris[i][i];
        }

        // Hata hesapla
        hata = 0.0;
        for (i = 0; i < n; i++) {
            hata += fabs(cozum[i] - onceki[i]);
        }

        // Hata toleransın altına düşerse döngüden çık
        if (hata < tol) {
            break;
        }
    }

    free(onceki);
}

void sayisalTurevIslemi() {
    char ifade[1024];
    double xDegeri, h;
    int turevTuru;

    printf("Denklemi giriniz (x değişkenini kullanarak, örn: 'x^2 - 4*x + 4'): ");
    fgets(ifade, sizeof(ifade), stdin);
    ifade[strcspn(ifade, "\n")] = 0;  // Newline karakterini temizle

    printf("Türev alınacak x değeri: ");
    scanf("%lf", &xDegeri);
    while((getchar()) != '\n');
    printf("Küçük değişim miktarı h: ");
    scanf("%lf", &h);
    while((getchar()) != '\n');
    printf("Türev tipini seçiniz (1: İleri, 2: Geri, 3: Merkezi): ");
    scanf("%d", &turevTuru);
    while((getchar()) != '\n');

    double turevSonucu = sayisalTurev(ifade, xDegeri, h, turevTuru);
    printf("Türevin sonucu: %.12f\n", turevSonucu);
}

void menu() {
    int secim;
    do {
        printf("\n*** Sayısal Yöntemler ve Interpolasyon ***\n");
        printf("1. Bisection Yöntemi\n");
        printf("2. Regula-Falsi Yöntemi\n");
        printf("3. Newton-Raphson Yöntemi\n");
        printf("4. NxN'lik Matrisin Tersi\n");
        printf("5. Gauss Eliminasyon\n");
        printf("6. Gauss-Seidel Yöntemi\n");
        printf("7. Sayısal Türev (Merkezi, İleri, Geri)\n");
        printf("8. Simpson Yöntemi\n");
        printf("9. Trapez Yöntemi\n");
        printf("10. Gregory-Newton Interpolasyonu\n");
        printf("0. Çıkış\n");
        printf("Seçiminizi yapınız: ");
        scanf("%d", &secim);
        getchar();  // Yeni satır karakterini tüketmek için

        switch (secim) {
            case 1:
                bisectionYontemi();
                break;
            case 2:
                regulaFalsiYontemi();
                break;
            case 3:
                newtonRaphsonYontemi();
                break;
            case 4:
                matrisTersiniBulmaIslemi();
                break;
            case 5:
                gaussEliminasyonIslemi();
                break;
            case 6:
                gaussSeidelIslemi();
                break;
            case 7:
                sayisalTurevIslemi();
                break;
            case 8:
                simpsonYontemi();
                break;
            case 9:
                trapezYontemi();
                break;
            case 10:
                interpolasyonIslemi();
                break;
            case 0:
                printf("Programdan çıkılıyor...\n");
                break;
            default:
                printf("Geçersiz seçim! Lütfen tekrar deneyin.\n");
                break;
        }
    } while (secim != 0);
}
