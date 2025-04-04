package cn.opensrcdevelop.common.util;

import cn.opensrcdevelop.common.exception.ServerException;
import cn.opensrcdevelop.common.exception.ValidationException;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.google.zxing.BarcodeFormat;
import com.google.zxing.EncodeHintType;
import com.google.zxing.client.j2se.MatrixToImageWriter;
import com.google.zxing.common.BitMatrix;
import com.google.zxing.qrcode.QRCodeWriter;
import io.vavr.control.Try;
import jakarta.validation.ConstraintViolation;
import jakarta.validation.Validator;
import org.apache.commons.codec.binary.Base32;
import org.apache.commons.codec.binary.Base64;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.util.ReflectionUtils;

import java.io.*;
import java.lang.invoke.SerializedLambda;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.SecureRandom;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Stream;

@SuppressWarnings("unused")
public class CommonUtil {

    private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();
    private static final SecureRandom SECURE_RANDOM = new SecureRandom();
    private static final Base32 BASE32 = new Base32();
    private static final Base64 BASE64 = new Base64();

    static {
        OBJECT_MAPPER.disable(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS);
        OBJECT_MAPPER.registerModule(new JavaTimeModule());
        OBJECT_MAPPER.setSerializationInclusion(JsonInclude.Include.NON_NULL);
    }

    private CommonUtil() {
    }

    /**
     * JDK序列化对象
     *
     * @param object 对象
     * @return Base64编码字符串
     */
    public static String javaSerialize(Object object) {
        return Try.of(() -> {
            ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
            try (ObjectOutputStream objectOutputStream = new ObjectOutputStream(byteArrayOutputStream)) {
                objectOutputStream.writeObject(object);
                return Base64.encodeBase64String(byteArrayOutputStream.toByteArray());
            }
        }).getOrElseThrow(ServerException::new);
    }

    /**
     * JDK反序列化对象
     *
     * @param base64Str Base64编码字符串
     * @return 对象
     */
    @SuppressWarnings("unchecked")
    public static <T> T javaDeserialize(String base64Str) {
        return Try.of(() -> {
            ByteArrayInputStream byteArrayInputStream = new ByteArrayInputStream(Base64.decodeBase64(base64Str));
            try (ObjectInputStream objectInputStream = new ObjectInputStream(byteArrayInputStream)) {
                return (T) objectInputStream.readObject();
            }
        }).getOrElseThrow(ServerException::new);
    }

    /**
     * 获取随机Base32编码的字符串
     *
     * @param bytes 字节数
     * @return 随机Base32编码的字符串
     */
    public static String getBase32StringKey(int bytes) {
        if (bytes <= 0) {
            return StringUtils.EMPTY;
        }
        byte[] randomBytes = new byte[bytes];
        SECURE_RANDOM.nextBytes(randomBytes);
        return BASE32.encodeAsString(randomBytes);
    }

    /**
     * 获取随机Base64编码的字符串
     *
     * @param bytes 字节数
     * @return 随机Base64编码的字符串
     */
    public static String getBase64StringKey(int bytes) {
        if (bytes <= 0) {
            return StringUtils.EMPTY;
        }
        byte[] randomBytes = new byte[bytes];
        SECURE_RANDOM.nextBytes(randomBytes);
        return BASE64.encodeAsString(randomBytes);
    }

    /**
     * 序列化对象
     *
     * @param obj 对象
     * @return 序列化字符串
     */
    public static String serializeObject(Object obj) {
        String val = null;
        try {
            // json 序列化
            val = OBJECT_MAPPER.writeValueAsString(obj);
        } catch (JsonProcessingException e) {
            // jdk 序列化
            val = javaSerialize(obj);
        }
        return val;
    }

    /**
     * 反序列化对象
     *
     * @param value 反序列化字符串
     * @return 对象
     */
    public static <T> T deserializeObject(String value, Class<T> clazz) {
        try {
            // json 反序列化
            return OBJECT_MAPPER.readValue(value, clazz);
        } catch (JsonProcessingException e) {
            // jdk 反序列化
            return javaDeserialize(value);
        }
    }

    /**
     * 反序列化对象
     *
     * @param value 反序列化字符串
     * @return 对象
     */
    public static <T> T deserializeObject(String value, TypeReference<T> valueTypeRef) {
        try {
            // json 反序列化
            return OBJECT_MAPPER.readValue(value, valueTypeRef);
        } catch (JsonProcessingException e) {
            // jdk 反序列化
            return javaDeserialize(value);
        }
    }

    /**
     * 获取UUID字符串
     *
     * @return UUID字符串
     */
    public static String getUUIDString() {
        return UUID.randomUUID().toString();
    }

    /**
     * 流判空处理
     *
     * @param collection 集合
     * @param <T>        T
     * @return 流
     */
    public static <T> Stream<T> stream(Collection<T> collection) {
        if (CollectionUtils.isNotEmpty(collection)) {
            return collection.stream();
        }
        return Stream.empty();
    }

    /**
     * 生成二维码（PNG）
     *
     * @param width  宽
     * @param height 高
     * @param data   数据
     * @return 二维码（PNG）
     */
    public static byte[] generatePngQrCode(int width, int height, String data) {
        try {
            Map<EncodeHintType, Object> hints = new EnumMap<>(EncodeHintType.class);
            hints.put(EncodeHintType.CHARACTER_SET, StandardCharsets.UTF_8.name());
            hints.put(EncodeHintType.MARGIN, 1);

            QRCodeWriter qrCodeWriter = new QRCodeWriter();
            BitMatrix bitMatrix = qrCodeWriter.encode(data, BarcodeFormat.QR_CODE, width, height, hints);
            ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
            MatrixToImageWriter.writeToStream(bitMatrix, "PNG", byteArrayOutputStream);
            return byteArrayOutputStream.toByteArray();
        } catch (Exception e) {
            throw new ServerException(e);
        }
    }

    /**
     * 生成二维码（PNG）
     *
     * @param width  宽
     * @param height 高
     * @param data   数据
     * @return 二维码（PNG）
     */
    public static String getBase64PngQrCode(int width, int height, String data) {
        String imageFormat = "data:image/png;base64,%s";
        return String.format(imageFormat, new Base64().encodeAsString(generatePngQrCode(width, height, data)));
    }

    /**
     * 调用 Set 方法前进行检查
     *
     * @param predicate 检查方法
     * @param setMethod set 方法
     * @param supplier  目标对象
     * @param <T>       T
     */
    public static <T> void callSetWithCheck(Predicate<T> predicate, Consumer<T> setMethod, Supplier<T> supplier) {
        if (predicate.test(supplier.get())) {
            setMethod.accept(supplier.get());
        }
    }

    /**
     * 从 Getter 方法引用提取字段名
     *
     * @param getter Getter 方法
     * @param <T>    T
     * @param <R>    R
     * @return 字段名
     */
    public static <T, R> String extractFileNameFromGetter(Getter<T, R> getter) {
        try {
            // 反射获取 writeReplace 方法
            Method writeReplace = getter.getClass().getDeclaredMethod("writeReplace");
            ReflectionUtils.makeAccessible(writeReplace);
            SerializedLambda serializedLambda = (SerializedLambda) writeReplace.invoke(getter);
            // 获取实现方法，也就是方法引用对应的方法名
            String methodName = serializedLambda.getImplMethodName();

            if (methodName.startsWith("is")) {
                return StringUtils.uncapitalize(methodName.substring(2));
            } else if (methodName.startsWith("get")) {
                return StringUtils.uncapitalize(methodName.substring(3));
            } else {
                throw new IllegalArgumentException("method name should start with 'is' or 'get'");
            }
        } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {
            throw new IllegalArgumentException(e);
        }
    }

    /**
     * 生成 RSA 密钥对
     *
     * @return RSA 密钥对
     */
    public static KeyPair generateRsaKey() {
        KeyPair keyPair;
        try {
            KeyPairGenerator keyPairGenerator = KeyPairGenerator.getInstance("RSA");
            keyPairGenerator.initialize(2048);
            keyPair = keyPairGenerator.generateKeyPair();
        } catch (Exception ex) {
            throw new IllegalStateException(ex);
        }
        return keyPair;
    }

    /**
     * 将时间戳转换为指定格式的字符串
     *
     * @param timestamp 时间戳
     * @param format    格式
     * @return 字符串
     */
    public static String convertTimestamp2String(long timestamp, String format) {
        return LocalDateTime.ofInstant(Instant.ofEpochMilli(timestamp), ZoneId.systemDefault()).format(DateTimeFormatter.ofPattern(format));
    }

    /**
     * 校验 Bean
     *
     * @param bean bean
     * @param groups 分组
     */
    public static void validateBean(Object bean, Class<?>... groups) {
        Validator validator = SpringContextUtil.getBean(Validator.class);
        Set<ConstraintViolation<Object>> constraintViolations = validator.validate(bean, groups);
        if (CollectionUtils.isNotEmpty(constraintViolations)) {
            throw new ValidationException(constraintViolations);
        }
    }

    @FunctionalInterface
    public interface Getter<T, R> extends Serializable {
        R get(T t);
    }
}
