package br.unb.cdt.desafioPositivo.util.criptografia;

import java.security.MessageDigest;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.PBEParameterSpec;

/**
 * Classe utilitaria que disponibiliza metodos para criptografar texto,
 * particularmente senhas de usuarios. Essa implementacao eh baseada no algoritmo
 * descrito em:
 * 
 * http://download.oracle.com/javase/6/docs/technotes/guides/security/crypto/
 * CryptoSpec.html#SimpleEncrEx
 * 
 * @author rbonifacio
 */
public class CriptografiaUtil {

	private static final String PADRAO_SENHA_VALIDA = "^.*(?=.{8,})(?=.*\\d)(?=.*[a-z])(?=.*[A-Z]).*$";
	
	private static byte[] salt = { (byte) 0xc7, (byte) 0x73, (byte) 0x21,
			(byte) 0x8c, (byte) 0x7e, (byte) 0xc8, (byte) 0xee, (byte) 0x99 };
	
	
	private static int count = 20;

	public static String criptografar(String s) throws Exception {
		PBEParameterSpec pbeParameterSpec = new PBEParameterSpec(salt, count);
		PBEKeySpec pbeKeySpec = new PBEKeySpec(s.toCharArray());
		SecretKeyFactory keyFactory = SecretKeyFactory
				.getInstance("PBEWithMD5AndDES");
		SecretKey pbeKey = keyFactory.generateSecret(pbeKeySpec);

		Cipher pbeCipher = Cipher.getInstance("PBEWithMD5AndDES");

		pbeCipher.init(Cipher.ENCRYPT_MODE, pbeKey, pbeParameterSpec);

		return new String(pbeCipher.doFinal());
	}

	public static String criptografarMD5(String s) throws Exception {
		MessageDigest md = MessageDigest.getInstance("MD5");
		md.update(s.getBytes());
		byte[] hashMd5 = md.digest();
		
		return stringHexa(hashMd5);
	}
	
	public static boolean verificaSenha(String senha) {
		Pattern p = Pattern.compile(PADRAO_SENHA_VALIDA);
		Matcher matcher = p.matcher(senha);
		return matcher.matches();
	}

	private static String stringHexa(byte[] bytes) {
		StringBuilder s = new StringBuilder();
		for(int i = 0; i < bytes.length; i++) {
			int parteAlta = ((bytes[i] >> 4) & 0xf) << 4;
			int parteBaixa = bytes[i] & 0xf;
			if (parteAlta == 0)
				s.append('0');
			s.append(Integer.toHexString(parteAlta | parteBaixa));
		}
		return s.toString();
	}
}
