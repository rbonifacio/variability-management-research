package br.unb.cdt.desafioPositivo.model;

import java.io.Serializable;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.hibernate.validator.AssertTrue;
import org.hibernate.validator.Email;
import org.hibernate.validator.NotNull;
import org.hibernate.validator.Length;
import org.hibernate.validator.Past;

import br.unb.cdt.desafioPositivo.model.acesso.AcessoUsuario;

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
public class Usuario implements Serializable {
	
	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long id;
	
	private String nome;
	
	private String sobrenome;
	
	@Enumerated(EnumType.ORDINAL)
	private Sexo sexo;
	
	@Email(message="positivo.validacao.email")
	@Column(unique=true)
	private String email;
	
	@Enumerated(EnumType.ORDINAL)
	private Estado estado; 
	
	@Past
	private Date nascimento;
	
	private String token;
	
	@OneToMany(mappedBy="usuario", cascade=CascadeType.ALL)
	private List<Proposta> propostas;
	
	@OneToMany(mappedBy="usuario",cascade=CascadeType.ALL)
	private List<AcessoUsuario> historicoSituacaoAcesso;
	
	
	@Transient
	private String senha;
	
	@Transient
	private String confirmacaoSenha;
	
	@Transient
	private String confirmacaoEmail;
	
	@Transient
	private String codigoConfirmacaoCadastro;
	
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

	@NotNull
	@Length(max=50)
	public String getNome() {
		return nome;
	}

	public void setNome(String nome) {
		this.nome = nome;
	}

	@NotNull
	@Length(max=50)
	public String getSobrenome() {
		return sobrenome;
	}

	public void setSobrenome(String sobrenome) {
		this.sobrenome = sobrenome;
	}

	@NotNull
	public Sexo getSexo() {
		return sexo;
	}

	public void setSexo(Sexo sexo) {
		this.sexo = sexo;
	}

	@NotNull
	public String getEmail() {
		return email;
	}

	public void setEmail(String email) {
		this.email = email;
	}

	@Length(min=5, max=15)
	public String getSenha() {
		return senha;
	}

	public void setSenha(String senha) {
		this.senha = senha;
	}

	@Length(min=5, max=15)
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



	public String getConfirmacaoEmail() {
		return confirmacaoEmail;
	}

	public void setConfirmacaoEmail(String confirmacaoEmail) {
		this.confirmacaoEmail = confirmacaoEmail;
	}

	public String getCodigoConfirmacaoCadastro() {
		return codigoConfirmacaoCadastro;
	}

	public void setCodigoConfirmacaoCadastro(String codigoConfirmacaoCadastro) {
		this.codigoConfirmacaoCadastro = codigoConfirmacaoCadastro;
	}

	public String getToken() {
		return token;
	}

	public void setToken(String token) {
		this.token = token;
	}

	public List<AcessoUsuario> getHistoricoSituacaoAcesso() {
		if(historicoSituacaoAcesso == null) {
			historicoSituacaoAcesso = new ArrayList<AcessoUsuario>();
		}
		return historicoSituacaoAcesso;
	}

	public void setHistoricoSituacaoAcesso(List<AcessoUsuario> historicoSituacaoAcesso) {
		this.historicoSituacaoAcesso = historicoSituacaoAcesso;
	}
	
	public AcessoUsuario getSituacaoAcessoAtual() {
		for(AcessoUsuario a: historicoSituacaoAcesso) {
			if(a.getDataFim() == null) {
				return a;
			}
		}
		return null;
	}
	
	@AssertTrue(message="positivo.confirmacao.email.invalido")
	public boolean validaConfirmacaoEmail() {
		return email.equals(confirmacaoEmail);
	}
}
