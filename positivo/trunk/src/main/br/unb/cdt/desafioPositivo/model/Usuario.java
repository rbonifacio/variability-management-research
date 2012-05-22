package br.unb.cdt.desafioPositivo.model;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.jboss.seam.annotations.Name;

/**
 * Classe que representa um proponente do 
 * desafio Positivo para o desenvolvimento de 
 * aplicacoes Android.
 * 
 * @author positivo
 *
 */
@Entity
@Table(name="TB_USUARIO")
@Name("usuario")
public class Usuario {
	
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long id;
	
	private String nome;
	
	private String sobrenome;
	
	@Enumerated(EnumType.ORDINAL)
	private Sexo sexo;
	
	private String email;
	
	@Enumerated(EnumType.ORDINAL)
	private Estado estado; 
	
	private Date nascimento;
	
	@OneToMany(mappedBy="usuario", cascade=CascadeType.ALL)
	private List<Proposta> propostas;
	
	@Transient
	private String senha;
	
	@Transient
	private String confirmacaoSenha;
	
	/**
	 * Necessario, de acordo com a especificao 
	 * JPA.
	 */
	public Usuario() {
		nascimento = new Date();
	}

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getNome() {
		return nome;
	}

	public void setNome(String nome) {
		this.nome = nome;
	}

	public String getSobrenome() {
		return sobrenome;
	}

	public void setSobrenome(String sobrenome) {
		this.sobrenome = sobrenome;
	}

	public Sexo getSexo() {
		return sexo;
	}

	public void setSexo(Sexo sexo) {
		this.sexo = sexo;
	}

	public String getEmail() {
		return email;
	}

	public void setEmail(String email) {
		this.email = email;
	}

	public String getSenha() {
		return senha;
	}

	public void setSenha(String senha) {
		this.senha = senha;
	}

	public String getConfirmacaoSenha() {
		return confirmacaoSenha;
	}

	public void setConfirmacaoSenha(String confirmacaoSenha) {
		this.confirmacaoSenha = confirmacaoSenha;
	}
	
	public Date getNascimento() {
		return nascimento;
	}

	public void setNascimento(Date nascimento) {
		this.nascimento = nascimento;
	}

	public Estado getEstado() {
		return estado;
	}

	public void setEstado(Estado estado) {
		this.estado = estado;
	}

	public List<Proposta> getPropostas() {
		return propostas;
	}

	public void setPropostas(List<Proposta> propostas) {
		this.propostas = propostas;
	}
	
	public String getNascimentoFormatado() {
		DateFormat format = new SimpleDateFormat("dd/MM/yyyy");
		return format.format(nascimento);
	}


	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime
				* result
				+ ((confirmacaoSenha == null) ? 0 : confirmacaoSenha.hashCode());
		result = prime * result + ((email == null) ? 0 : email.hashCode());
		result = prime * result + ((id == null) ? 0 : id.hashCode());
		result = prime * result + ((nome == null) ? 0 : nome.hashCode());
		result = prime * result + ((senha == null) ? 0 : senha.hashCode());
		result = prime * result + ((sexo == null) ? 0 : sexo.hashCode());
		result = prime * result
				+ ((sobrenome == null) ? 0 : sobrenome.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if(obj instanceof Usuario) {
			return ((Usuario)obj).getId().equals(this.getId());
		}
		return false;
	}
	
	/**
	 * Converte um usuario para o formato JSON. Ok, eu poderia 
	 * usar uma API para isso. Mas estou aos 50 minutos do segundo 
	 * tempo. 
	 * 
	 * @return Representacao JSON de um usuario. 
	 */
	public String toJson() {
		DateFormat format = new SimpleDateFormat("dd/MM/yyyy");
		String nasimentoComoString = format.format(nascimento);
		
	
		return "\"email\" : " + email + "," +
			   "\"senha\" : " + senha + "," + 	 
			   "\"nome\": "   + nome  + "," +
			   "\"sobrenome\": " + sobrenome + "," +   
			   "\"sexo\" :" + "M" + "," +//(sexo.equals(Sexo.MASCULINO) ? "M" : "F") + "," + 
			   "\"dataNascimento\" : " + nasimentoComoString + "," +
			   "\"estado\" : " + "PE" + "," + 
			   "\"status\" :  " + "A";
	}


	
}
