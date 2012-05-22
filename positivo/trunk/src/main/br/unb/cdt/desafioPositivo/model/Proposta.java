package br.unb.cdt.desafioPositivo.model;

import javax.persistence.Basic;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.Lob;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import org.jboss.seam.annotations.Name;

/**
 * Classes que representa uma proposta 
 * submetida ao desafio Positivo para desenvolvimento 
 * de aplicacoes Android.
 * 
 * @author rbonifacio
 *
 */
@Entity
@Table(name="TB_PROPOSTA")
@Name("proposta")
public class Proposta {
	
	@Id
	@GeneratedValue(strategy=GenerationType.IDENTITY)
	private Long id;

	private String nome;
	
	private String descricao;
	
	private String objetivos;
	
	@ManyToOne
	private Usuario usuario;
	
	@Enumerated(EnumType.ORDINAL)
	private Categoria categoria;
	
	private String publicoAlvo;
	
	private String descricaoFuncional;
	
	@Basic(fetch=FetchType.LAZY)
	@Lob
	private byte[] arquivoGUI;
	
	/**
	 * Necessario, de acordo com a especificacao JPA.
	 */
	public Proposta() {}

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

	public String getDescricao() {
		return descricao;
	}

	public void setDescricao(String descricao) {
		this.descricao = descricao;
	}

	public Categoria getCategoria() {
		return categoria;
	}

	public void setCategoria(Categoria categoria) {
		this.categoria = categoria;
	}

	public String getPublicoAlvo() {
		return publicoAlvo;
	}

	public void setPublicoAlvo(String publicoAlvo) {
		this.publicoAlvo = publicoAlvo;
	}

	public String getDescricaoFuncional() {
		return descricaoFuncional;
	}

	public void setDescricaoFuncional(String descricaoFuncional) {
		this.descricaoFuncional = descricaoFuncional;
	}

	public byte[] getArquivoGUI() {
		return arquivoGUI;
	}

	public void setArquivoGUI(byte[] arquivoGUI) {
		this.arquivoGUI = arquivoGUI;
	}

	public String getObjetivos() {
		return objetivos;
	}

	public void setObjetivos(String objetivos) {
		this.objetivos = objetivos;
	}

	public Usuario getUsuario() {
		return usuario;
	}

	public void setUsuario(Usuario usuario) {
		this.usuario = usuario;
	} 
}
